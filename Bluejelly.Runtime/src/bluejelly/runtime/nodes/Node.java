/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * A heap node. Heap nodes can be <em>entered</em>, 
 * that is, evaluated to normal form.
 * 
 * @author ppedemon
 */
public interface Node {
    
    /**
     * Enter this node, evaluating it to normal form.
     * @param ctx    {@link ExecutionContext} where this node is executing
     */
    void enter(ExecutionContext ctx);
    
}
