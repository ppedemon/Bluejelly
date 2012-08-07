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
 * Interface for nodes that model executable code (that is, a function)
 * declared in a {@link Module}.
 * 
 * @author ppedemon
 */
public interface Executable extends Node {

    /**
     * Get function arity.
     * @return    function arity
     */
    int getArity();
    
    /**
     * Get module defining the function.
     * @return   module defining the function 
     */
    Module getModule();
    
    /**
     * Get function name.
     * @return    function name
     */
    String getFunctionName();

    /**
     * Whether this function is a continuation doing pattern matching over
     * a type constructor assumed to be on top of the stack. This allows us 
     * to implement the &quot;return in registers&quot; convention.
     *  
     * @return    
     *   <code>true</code> if this function is a tycon pattern 
     *   matching continuation, <code>false</code> otherwise
     */
    boolean isMatcher();
    
    /**
     * Sometimes, functions can be entered in a faster way.
     * A typical case is when we statically know that the
     * number of arguments for a function matches its arity.
     *  
     * @param ctx    {@link ExecutionContext} where the function will execute
     */
    void fastEnter(ExecutionContext ctx);
}
