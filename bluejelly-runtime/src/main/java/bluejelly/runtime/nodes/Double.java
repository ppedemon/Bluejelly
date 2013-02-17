/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * Node holding a double precision floating point value.
 * @author ppedemon
 */
public class Double implements Node {

    public double d;
    
    /**
     * Construct a node for the given number.
     * @param d    value for the node
     */
    public Double(double d) {
        this.d = d;
    }
    
    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void enter(ExecutionContext ctx) {
        ctx.whnf();
    }

    @Override
    public String toString() {
        return String.valueOf(d);
    }

}
