/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime;

import java.io.IOException;
import java.util.LinkedList;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.EmptyVisitor;
import org.objectweb.asm.commons.Method;

import bluejelly.runtime.ann.JellyCode;

/**
 * Collect methods representing compiled functions from a given module.
 * Collect dictionaries declared in a given module.
 * 
 * @author ppedemon
 */
public class ModuleReader extends EmptyVisitor {
    
    /**
     * Code in a module is defined by the method implementing
     * it and its arity.
     */
    public static class CodeInfo {
        private int arity;
        private boolean matcher;
        private final Method method;
        
        public CodeInfo(Method method) {
            this(0, method);
        }
        
        public CodeInfo(int arity, Method method) {
            this.arity = arity;
            this.method = method;
        }

        public int getArity() {
            return arity;
        }

        public void setArity(int arity) {
            this.arity = arity;
        }
        
        public boolean isMatcher() {
            return this.matcher;
        }
        
        public void markAsMatcher() {
            this.matcher = true;
        }
        
        public Method getMethod() {
            return method;
        }
    }

    /**
     * Information extracted from a module: code + dictionaries
     */
    public static class ModuleInfo {
        private final CodeInfo[] functions;
        
        public ModuleInfo(CodeInfo[] functions) {
            this.functions = functions;
        }

        public CodeInfo[] getFunctions() {
            return functions;
        }
    }

    // Annotation to watch for
    private static final String JELLYCODE = 
        "L" + Type.getInternalName(JellyCode.class) + ';';
        
    // Last method seen
    private Method m;

    // Collect results here
    private final LinkedList<CodeInfo> code = new LinkedList<CodeInfo>();
    
    /**
     * Collect code for the given module.
     * 
     * @param module    module to collect code from
     * @return          info for those methods annotated with @JellyCode
     * 
     * @throws IOException    if access to class file fails
     */
    public ModuleInfo read(Module module) throws IOException {
        String name = module.getClass().getName();
        ClassReader r = new ClassReader(name);
        r.accept(this, 0);
        return new ModuleInfo(this.code.toArray(new CodeInfo[this.code.size()]));
    }

    /*
     * (non-Javadoc)
     * @see org.objectweb.asm.commons.EmptyVisitor#visitAnnotation(
     *   java.lang.String, boolean)
     */
    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        if (desc.equals(JELLYCODE)) {
            CodeInfo info = new CodeInfo(this.m);
            this.code.add(info);
        }
        return super.visitAnnotation(desc, visible);
    }

    /*
     * (non-Javadoc)
     * @see org.objectweb.asm.commons.EmptyVisitor#visit(
     *   java.lang.String, java.lang.Object)
     */
    @Override
    public void visit(String name, Object value) {
        if (name.equals("arity")) {
            CodeInfo info = this.code.getLast();
            info.setArity((Integer)value);
        } else if (name.equals("matcher")) {
            CodeInfo info = this.code.getLast();
            info.markAsMatcher();
        }
    }

    /*
     * (non-Javadoc)
     * @see org.objectweb.asm.commons.EmptyVisitor#visitMethod(
     *   int, java.lang.String, java.lang.String, java.lang.String, 
     *   java.lang.String[])
     */
    @Override
    public MethodVisitor visitMethod(
            int access, 
            String name, 
            String desc,
            String signature, 
            String[] exceptions) {
        
        Type[] paramTypes = Type.getArgumentTypes(desc);
        Type returnType = Type.getReturnType(desc);
        this.m = new Method(name, returnType, paramTypes);

        return super.visitMethod(access, name, desc, signature, exceptions);
    }
}
