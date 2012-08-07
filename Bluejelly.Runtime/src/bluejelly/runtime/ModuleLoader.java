package bluejelly.runtime;

import static org.objectweb.asm.Opcodes.ACC_FINAL;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.CHECKCAST;
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.INVOKEVIRTUAL;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.V1_6;

import java.lang.reflect.Constructor;
import java.util.HashSet;
import java.util.Set;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

import bluejelly.runtime.ModuleReader.CodeInfo;
import bluejelly.runtime.ModuleReader.DictInfo;
import bluejelly.runtime.ModuleReader.ModuleInfo;
import bluejelly.runtime.nodes.Caf;
import bluejelly.runtime.nodes.Code;
import bluejelly.runtime.nodes.Dict;

/**
 * Loads a single module.
 * 
 * <p>
 * <em>NB:</em> This class is <em>NOT</em> thread-safe. It's 
 * responsibility of the users of this class (the {@link Runtime}) 
 * to properly ensure thread-safety.
 * 
 * @author ppedemon
 */
public class ModuleLoader {

    /**
     * Ad-hoc, trivial class loader.
     */
    private static class CodeLoader extends ClassLoader {

        public CodeLoader(ClassLoader parent) {
            super(parent);
        }
        
        public Class<?> defineClass(String name, byte[] b) {
            String normalizedName = name.replace('/', '.');
            return this.defineClass(normalizedName , b, 0, b.length);
        }
    }

    // We use this to generate a unique prefix for name of generated classes
    private static int count = 0;
    
    // Package where generated code will live
    private static final String CODE_PKG = "org/bluejelly/code";
    
    // Name prefix for all generated commands
    private static final String CODE_PREFIX = "C";

    private final Runtime runtime;
    private final CodeLoader loader;
    private final Set<String> loaded;
    
    /**
     * Default constructor.
     */
    public ModuleLoader(Runtime runtime) {
        this.runtime = runtime;
        this.loader = new CodeLoader(this.getClass().getClassLoader());
        this.loaded = new HashSet<String>();
    }
    
    /**
     * Was the given module loaded?
     * @param moduleName    name of module to look for
     * @return              whether module was or not loaded
     */
    public boolean isLoaded(String moduleName) {
        return loaded.contains(moduleName);
    }
    
    /**
     * Load the given module into the runtime. 
     * <b>Precondition:</b> modules wasn't loaded before
     * 
     * @param moduleName    name of module to load
     * @throws JellyException    if loading fails
     */
    @SuppressWarnings("unchecked")
    public void load(String moduleName) throws JellyException {
        
        // Nothing to do if we already loaded this
        if (this.loaded.contains(moduleName))
            return;
        
        try {
            // Instantiate module
            Class<Module> mcls = (Class<Module>)Class.forName(moduleName);
            Constructor<Module> cons = mcls.getConstructor();
            Module module = cons.newInstance();
            
            // Read code and dictionaries from a module
            ModuleReader c = new ModuleReader();
            ModuleInfo mInfo = c.read(module);
            
            // Register dictionaries in runtime
            for (DictInfo di : mInfo.getDictionaries()) {
                Dict dictNode = new Dict(module, di.getName(), di.getDict());
                this.runtime.register(IdUtils.qualify(module, di.getName()), dictNode);
            }
            
            // Register code in runtime
            for (CodeInfo ci : mInfo.getFunctions()) {
                if (ci.getArity() == 0) {
                    Class<Caf> clazz = this.generateCaf(module, ci);
                    this.registerCaf(clazz, module, ci);
                } else {
                    Class<Code> clazz = this.generateCode(module, ci);
                    this.registerCode(clazz, module, ci);
                }
            }
            
            this.loaded.add(moduleName);
        } catch (Exception e) {
            throw new JellyException(e);
        }
    }
    
    /**
     * Generate a {@link Caf} subclass for the given method information.
     * 
     * @param module    module declaring the method to process
     * @param ci        code info for the method to process
     * 
     * @return
     *   a {@link Caf} subclass whose <code>fastEnter</code> will call
     *   module.ci.getMethod() with an {@link ExecutionContext} as argument.
     */
    @SuppressWarnings("unchecked")
    private Class<Caf> generateCaf(Module module, CodeInfo ci) {
        String nodeName = this.genNodeName(ci);
        String cafName = Type.getInternalName(Caf.class);
        String moduleName = Type.getInternalName(module.getClass());
        String methodName = ci.getMethod().getName();

        ClassWriter cw = new ClassWriter(0);
        cw.visit(V1_6, ACC_PUBLIC + ACC_FINAL, nodeName, null, cafName, null);

        {
            String consDesc = Type.getMethodDescriptor(Type.VOID_TYPE, new Type[]{
                Type.getType(Module.class), 
                Type.getType(String.class)
            });
            
            MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", consDesc, null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitMethodInsn(INVOKESPECIAL, cafName, "<init>", consDesc);
            mv.visitInsn(RETURN);
            mv.visitMaxs(3, 3);
            mv.visitEnd();
        }

        this.generateFastEnter(cw, cafName, moduleName, methodName);
        
        cw.visitEnd();
        
        byte[] b = cw.toByteArray();
        Class<?> clazz = this.loader.defineClass(nodeName, b);
        return (Class<Caf>)clazz;
    }
    
    /**
     * Generate a {@link Code} subclass for the given method information.
     * 
     * @param module    module declaring the method to process
     * @param ci        code info for the method to process
     * 
     * @return          
     *   a {@link Code} subclass whose <code>fastEnter</code> will call
     *   module.ci.getMethod() with an {@link ExecutionContext} as argument.
     */
    @SuppressWarnings("unchecked")
    private Class<Code> generateCode(Module module, CodeInfo ci) {
        String nodeName = this.genNodeName(ci);
        String codeName = Type.getInternalName(Code.class);
        String moduleName = Type.getInternalName(module.getClass());
        String methodName = ci.getMethod().getName();
        
        ClassWriter cw = new ClassWriter(0);
        cw.visit(V1_6, ACC_PUBLIC + ACC_FINAL, nodeName, null, codeName, null);
        
        {
            String consDesc = Type.getMethodDescriptor(Type.VOID_TYPE, new Type[]{
                Type.getType(int.class), 
                Type.getType(Module.class), 
                Type.getType(String.class)
            });
            
            MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", consDesc, null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ILOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitVarInsn(ALOAD, 3);
            mv.visitMethodInsn(INVOKESPECIAL, codeName, "<init>", consDesc);
            mv.visitInsn(RETURN);
            mv.visitMaxs(4, 4);
            mv.visitEnd();
        }
        
        this.generateFastEnter(cw, codeName, moduleName, methodName);
        
        cw.visitEnd();
        
        byte[] b = cw.toByteArray();
        Class<?> clazz = this.loader.defineClass(nodeName, b);
        return (Class<Code>)clazz;
    }

    /**
     * Override <code>fastEnter</code> method in class being visited
     * by <code>cw</code>, subclass of <code>superName</code>. Method
     * will call <code>methodName</code>. declared in class 
     * <code>moduleName</code>. So , assuming class generated by
     * <code>cw</code> is named <code>X</code>, we will have:
     * 
     * <p>
     * <code>
     * class X extends superName {
     *     public void fastEnter(ExecutionContext ctx) {
     *         ((moduleName)this.module).methodName(ctx);
     *     }
     * }
     * </code>
     * 
     * @param cw            class writer generating class code
     * @param superName     internal name of superclass: Code or Caf
     * @param moduleName    internal name of module class
     * @param methodName    name of method to invoke in module moduleName
     */
    private void generateFastEnter(
            ClassWriter cw, 
            String superName, 
            String moduleName, 
            String methodName) {
        
        String fieldDesc= Type.getDescriptor(Module.class);
        String methodDesc = "(" + Type.getDescriptor(ExecutionContext.class) + ")V";
        
        MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "fastEnter", methodDesc, null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, superName, "module", fieldDesc);
        mv.visitTypeInsn(CHECKCAST, moduleName);
        mv.visitVarInsn(ALOAD, 1);
        mv.visitMethodInsn(INVOKEVIRTUAL, moduleName, methodName, methodDesc);
        mv.visitInsn(RETURN);
        mv.visitMaxs(2, 2);
        mv.visitEnd();        
    }
    
    /**
     * Register the given {@link Caf} node.
     * 
     * @param clazz    class for caf node to register
     * @param m        module where caf comes from
     * @param ci       code info for node to register
     * 
     * @throws JellyException  if code class can't be instantiated
     */
    private void registerCaf(Class<Caf> clazz, Module m, CodeInfo ci) 
            throws JellyException {
        
        try {
            String functionName = ci.getMethod().getName();
            Constructor<Caf> c = clazz.getConstructor(Module.class, String.class);
            Caf caf = c.newInstance(m, functionName);
            // If this caf is a tycon matcher continuation, mark it as such
            if (ci.isMatcher())
                caf.markAsMatcher();
            this.runtime.register(IdUtils.qualify(m, functionName), caf);
        } catch (Exception e) {
            throw new JellyException(e);
        }
    }

    /**
     * Register the given {@link Code} node.
     * 
     * @param clazz    class for code node to register
     * @param m        module where code comes from
     * @param ci       code info for node to register
     * 
     * @throws JellyException  if code class can't be instantiated
     */
    private void registerCode(Class<Code> clazz, Module m, CodeInfo ci) 
            throws JellyException {
        
        try {
            String functionName = ci.getMethod().getName();
            Constructor<Code> c = clazz.getConstructor(int.class, Module.class, String.class);
            Code code = c.newInstance(ci.getArity(), m, functionName);
            this.runtime.register(IdUtils.qualify(m, functionName), code);
        } catch (Exception e) {
            throw new JellyException(e);
        }
    }
    
    /**
     * Generate a unique name from the given code info object.
     * 
     * @param ci   input code info
     * @return     unique name
     */
    private String genNodeName(CodeInfo ci) {
        return new StringBuilder()
            .append(CODE_PKG)
            .append('/')
            .append(CODE_PREFIX)
            .append('$')
            .append(count++)
            .toString();
    }
}
