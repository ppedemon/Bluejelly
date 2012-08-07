/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;

/**
 * CAF node: no-arg executable node that can be updated.
 * 
 * CAF play two roles: genuine no-arg updatable functions and match 
 * continuations. In the first case, you must ensure an update marker 
 * will be pushed on the marker stack before entering the function. So, 
 * you always must enter them with <code>enter</code> rather than 
 * <code>fastEnter</code>, which means that you <em>can't</em> jump
 * to then. So we have:
 * 
 *   pushCode "caf" # ok
 *   jump     "caf" # wrong! CAF won't be updated
 * 
 * On the other hand, when a caf is used as a match continuation there's
 * nothing to update. You just want to jump to a function that will process
 * the matched value on the top of the stack. So, matchers are <em>never</em>
 * entered, you always jump to them. 
 * 
 * Overall, genuine cafs and matchers are dual views of a Caf node. On the
 * no-arg genuine caf case, you always do a pushCode. On the matcher case,
 * you always do a jump.
 * 
 * @author ppedemon
 */
public class Caf extends Updatable implements Executable {

    // Module providing the CAF
    protected final Module module;
    
    // CAF name
    private final String functionName;
    
    // Whether this CAF is used as a continuation for pattern
    // matching over a type constructor on top of the stack
    private boolean matcher;
    
    /**
     * Construct an instance representing a CAF named 
     * <code>functionName</code>, declared in module<code>module</code>.
     * 
     * @param module          module declaring the CAF
     * @param functionName    CAF name
     */
    public Caf(Module module, String functionName) {
        this.module = module;
        this.functionName = functionName;
    }
    
    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#getArity()
     */
    @Override
    public int getArity() {
        return 0;
    }

    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#getModule()
     */
    @Override
    public Module getModule() {
        return this.module;
    }

    /* (non-Javadoc)
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
        return this.matcher;
    }

    /**
     * Mark this CAF as a continuation that does pattern matching 
     * over a type constructor on top of the stack. Knowing this
     * allows us to implement the &quot;return in registers&quot; 
     * convention.
     */
    public void markAsMatcher() {
        this.matcher = true;
    }
    
    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Executable#fastEnter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void fastEnter(ExecutionContext ctx) {
        /*
         * As with Code nodes, subclasses will override this method
         * with a call to this.module.<functionName>(ctx). The real
         * works is done here.
         */
    }

    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Updatable#reduce(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void reduce(ExecutionContext ctx) {
        // Just pop this node from the stack and execute CAF's code
        ctx.s[ctx.sp--] = null;
        this.fastEnter(ctx);
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        //if (this.isUpdated()) {
        //    return this.getInd().toString();
        //} else {
            return new StringBuffer()
            .append("*<")
            .append(this.module.getClass().getName())
            .append(':')
            .append(this.functionName)
            .append('>')
            .toString();
        //}
    }

}
