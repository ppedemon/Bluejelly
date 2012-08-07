package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;

/**
 * Code nodes represent functions loaded from some module.
 * 
 * @author ppedemon
 */
public class Code implements Executable {

    // Function arity
    private final int arity;
        
    // Origin module
    protected final Module module;

    // Function in the origin module
    protected final String functionName;
    
    /**
     * Construct a code node. Code comes from the given module.
     * 
     * @param arity           function's arity
     * @param module          module where the code originated
     * @param functionName    function name
     */
    public Code(int arity, Module module, String functionName) {
        this.arity = arity;
        this.module = module;
        this.functionName = functionName;
    }

    /*
     * (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#getArity()
     */
    @Override
    public int getArity() {
        return this.arity;
    }
    
    /*
     * (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#getModule()
     */
    @Override
    public Module getModule() {
        return this.module;
    }
        
    /*
     * (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#getFunctionName()
     */
    @Override
    public String getFunctionName() {
        return this.functionName;
    }
    
    /*
     * (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#isTyconMatcher()
     */
    @Override
    public boolean isMatcher() {
        // Non-caf's aren't continuations
        return false;
    }

    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Node#enter()
     */
    @Override
    public void enter(ExecutionContext ctx) {
        boolean inWhnf = ctx.argCheck(this.arity);
        if (!inWhnf) {
            this.fastEnter(ctx);
        }
    }

    /*
     * (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#fastEnter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void fastEnter(ExecutionContext ctx) {
        /* Do the real work here, subclasses should call:
         *
         *    this.module.f(ctx)
         *
         * where f is a method annotated with @JellyCode
         * (that is, compiled code for a function in the module)
         */
    }

    @Override
    public String toString() {
        return new StringBuffer()
            .append('<')
            .append(this.module.getClass().getName())
            .append(':')
            .append(this.functionName)
            .append('>')
            .toString();
    }

}
