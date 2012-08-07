package bluejelly;

import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.INVOKEVIRTUAL;
import static org.objectweb.asm.Opcodes.RETURN;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.ann.JellyPrim;

/**
 * Take as input the name of a class declaring code annotated with
 * the {@link JellyPrim} annotation, and adds to it entry points 
 * and evaluators for each annotated method of the input class.
 *  
 * <p>You must invoke this class with a sequence of one or more class
 * <em>unqualified</em> class names. The class is assumed to live in
 * the same package as this one, i.e., <code>bluejelly</code>. For 
 * each method annotated with a {@link JellyPrim} in the given class,
 * we create as many methods as the arity of such primitive, each one 
 * evaluating one argument. For example, for the given input class:
 * 
 * <code>
 *   public class Prim implements Module {
 *     @JellyCode
 *     @JellyPrim(arity=2,name="somePrim")
 *     public void somePrimCode(ExecutionContext ctx) {...}
 *   }
 * </code>
 * 
 * when invoking <code>PrimGenerator Prim</code> we add to evaluators, 
 * thus obtaining:
 * 
 * <code>
 *   public class Prim implements Module {
 *     @JellyCode(arity=2)
 *     public void somePrim(ExecutionContext ctx) {
 *       stchk(2);
 *       ctx.evalVar(0,"bluejelly.Prim.somePrim$1");
 *     }
 *     @JellyCode
 *     public void somePrim$1(ExecutionContext ctx) {
 *       ctx.evalVar(2,"bluejelly.PrimStub.somePrimCode");
 *     }
 *     @JellyCode
 *     @JellyPrim(arity=2,name="somePrim")
 *     public void somePrimCode(ExecutionContext ctx) {...}
 *   }
 * </code>
 * 
 * @author ppedemon
 */
public class PrimTransformer extends ClassAdapter {
    
    /*
     * Helper class holding info about methods annotated as @JellyPrim.
     */
    private static class PrimInfo {
        String primName;
        int arity;
        String workerName;
        public PrimInfo(String primName, int arity, String workerName) {
            this.primName = primName;
            this.arity = arity;
            this.workerName = workerName;
        }
    }
    
    /*
     * Process @JellyPrim annotations.
     */
    private class JellyPrimProcessor implements AnnotationVisitor {
        private AnnotationVisitor av;
        
        private int arity;
        private String primName;
        
        public JellyPrimProcessor(AnnotationVisitor av) {
            this.av = av;
        }
        @Override
        public void visit(String name, Object value) {
            av.visit(name, value);
            if (name.equals("name")) {
                primName = (String)value;
            } else if (name.equals("arity")) {
                arity = ((Integer)value).intValue();
            }
        }
        @Override
        public AnnotationVisitor visitAnnotation(String name, String desc) {
            return av.visitAnnotation(name, desc);
        }
        @Override
        public AnnotationVisitor visitArray(String name) {
            return av.visitArray(name);
        }
        @Override
        public void visitEnd() {
            av.visitEnd();
            prims.add(new PrimInfo(primName, arity, currMethodName));
        }
        @Override
        public void visitEnum(String name, String desc, String value) {
            av.visitEnum(name, desc, value);
        }
    }
    
    /*
     * Check if a method is annotated as @JellyPrim.
     */
    private class CheckAdapter extends MethodAdapter {
        public CheckAdapter(MethodVisitor v) {
            super(v);
        }
        @Override
        public AnnotationVisitor visitAnnotation(String name, boolean visible) {
            AnnotationVisitor av = mv.visitAnnotation(name, visible);            
            if (name.equals(Type.getDescriptor(JellyPrim.class))) {
                return new JellyPrimProcessor(av);
            } else {
                return av;
            }
        }
    }
    
    // Map of 1-byte push int instructions
    private static final HashMap<Integer,Integer> 
    ICONSTS = new HashMap<Integer,Integer>();
    static {
        ICONSTS.put(-1, Opcodes.ICONST_M1);
        ICONSTS.put( 0, Opcodes.ICONST_0);
        ICONSTS.put( 1, Opcodes.ICONST_1);
        ICONSTS.put( 2, Opcodes.ICONST_2);
        ICONSTS.put( 3, Opcodes.ICONST_3);
        ICONSTS.put( 4, Opcodes.ICONST_4);
        ICONSTS.put( 5, Opcodes.ICONST_5);
    }

    // Convenience descriptor constants
    private static String CTX = 
            Type.getInternalName(ExecutionContext.class);
    private static String DESC = Type.getMethodDescriptor(
        Type.VOID_TYPE, new Type[]{Type.getType(ExecutionContext.class)});
    
    // Name of class being processed
    private String className;
    
    // Name of last method seen
    private String currMethodName;
    
    // List of info about methods annotated as @JellyPrim
    private List<PrimInfo> prims = new ArrayList<PrimInfo>();
    
    private PrimTransformer(ClassWriter w, String className) {
        super(w);
        this.className = className;
    }

    @Override
    public MethodVisitor visitMethod(
            int access, 
            String name, 
            String desc,
            String signature, 
            String[] exceptions) {
        currMethodName = name;
        MethodVisitor v = cv.visitMethod(access, name, desc, signature, exceptions);
        return new CheckAdapter(v);
    }

    @Override
    public void visitEnd() {
        for (PrimInfo pi: prims) {
            generate(pi);
        }
    }

    /*
     * Generate argument evaluators for each collected primitive.
     */
    private void generate(PrimInfo pi) {
        for (int i = 0; i < pi.arity; i++) {
            Label start = new Label();
            Label end = new Label();
            String method = getMethodName(pi, i);
            String cont = getContName(pi, i);
            MethodVisitor v = cv.visitMethod(ACC_PUBLIC, method, DESC, null, null);
            generateAnn(v, pi, i);
            v.visitCode();
            v.visitLabel(start);
            if (i == 0) {
                v.visitIntInsn(ALOAD, 1);
                pushIntConst(v, pi.arity);
                v.visitMethodInsn(INVOKEVIRTUAL, CTX, "stackCheck", "(I)V");
            }
            v.visitIntInsn(ALOAD, 1);
            pushIntConst(v, i << 1);
            v.visitLdcInsn(cont);
            v.visitMethodInsn(INVOKEVIRTUAL, CTX, "evalVar", "(ILjava/lang/String;)V");
            v.visitLabel(end);
            v.visitInsn(RETURN);
            v.visitLocalVariable("ctx", 
                Type.getDescriptor(ExecutionContext.class), null, start, end, 1);
            v.visitMaxs(3, 2);
            v.visitEnd();
        }
    }
    
    /*
     * Generate a @JellyCode annotation
     */
    private void generateAnn(MethodVisitor v, PrimInfo pi, int i) {
        AnnotationVisitor a = v.visitAnnotation(Type.getDescriptor(JellyCode.class), true);
        if (i == 0) a.visit("arity", pi.arity);
        a.visitEnd();
    }

    /* 
     * Efficiently push an integer value onto the JVM stack.
     */
    private void pushIntConst(MethodVisitor v, int n) {
        if (ICONSTS.containsKey(n)) {
            v.visitInsn(ICONSTS.get(n));
        } else if (n >= Byte.MIN_VALUE && n <= Byte.MAX_VALUE) {
            v.visitIntInsn(Opcodes.BIPUSH, n);
        } else if (n >= Short.MIN_VALUE && n <= Short.MAX_VALUE) {
            v.visitIntInsn(Opcodes.SIPUSH, n);
        } else {
            v.visitLdcInsn(n);
        }
    }

    /*
     * Get name of evaluator to generate.
     */
    private String getMethodName(PrimInfo pi, int i) {
        return pi.primName + (i == 0? "" : "$" + i);
    }

    /*
     * Get name of continuation to call.
     */
    private String getContName(PrimInfo pi, int i) {
        int j = i + 1;
        if (j == pi.arity) {
            return className + "." + pi.workerName;
        } else {
            return className + "." + pi.primName + "$" + j;
        }
    }

    /*
     * Save transformed class.
     */
    private void saveClass(String path, String className) throws IOException {
        byte[] bytes = ((ClassWriter)this.cv).toByteArray();
        File f = new File(path);
        f.mkdirs();
        FileOutputStream fos = 
            new FileOutputStream(f + "/" + className + ".class");
        try {
            fos.write(bytes);
        } finally {
            fos.close();
        }
    }

    /**
     * Execute the primitive generator. Example invocation:
     *   <code>PrimGenerator Prim</code>
     * 
     * @param args          command line arguments, not used
     * @throws Exception    if generated class can't be saved
     */
    public static void main(String[] args) throws Exception {
        if (args.length == 0) {
            System.err.println("Usage: PrimGenerator <primClassName>+");
            System.exit(1);
        }
        URL url = PrimTransformer.class.getResource(".");
        for (String className: args) {
            String pkgName = PrimTransformer.class.getPackage().getName();
            String qClassName = pkgName + "." + className;
            InputStream s = PrimTransformer.class.getResourceAsStream(className + ".class");
            ClassReader r = new ClassReader(s);
            ClassWriter w = new ClassWriter(r, 0);
            PrimTransformer a = new PrimTransformer(w, qClassName);
            r.accept(a, 0);
            a.saveClass(url.getPath(), className);
        }
    }
}
