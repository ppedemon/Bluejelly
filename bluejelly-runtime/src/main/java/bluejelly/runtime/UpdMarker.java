/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime;

import bluejelly.runtime.nodes.NApp;
import bluejelly.runtime.nodes.Node;
import bluejelly.runtime.nodes.Raise;
import bluejelly.runtime.nodes.Updatable;

/**
 * An update marker. Whenever found on top of the stack, it will
 * perform an update operation.
 * 
 * @author ppedemon
 */
public class UpdMarker extends Marker {

    // Node to update
    private final Updatable target;
    
    /**
     * Construct a new instance with the given {@link ExecutionContext}.
     * 
     * @param ctx     associated context
     * @param target  node to be updated when processing this marker
     */
    public UpdMarker(ExecutionContext ctx, Updatable target) {
        super(ctx);
        this.target = target;
    }

    /* 
     * So, the execution context found a partial application on top
     * of the stack, and this update marker is on top of the marker stack.
     * We must update the target node with the partial app, by turning
     * it into a NApp node first.
     * 
     * (non-Javadoc)
     * @see bluejelly.runtime.Marker#process()
     */
    @Override
    protected void signalPap() {
        int n = this.ctx.sp - this.ctx.bp;
        Node[] nodes = new Node[n];
        System.arraycopy(this.ctx.s, this.ctx.sp - n + 1, nodes, 0, n);
        NApp napp = new NApp(nodes);
        this.target.update(napp);
    }

    /*
     * So, the execution context has found a whnf node on top of the
     * stack, and this update marker is on top of the marker stack.
     * We must simply update the target node with the whnf node.
     * 
     * (non-Javadoc)
     * @see bluejelly.runtime.Marker#signalWhnf()
     */
    @Override
    protected void signalWhnf() {
        this.target.update(this.ctx.s[this.ctx.sp]);
    }

    /*
     * So, the execution context is in an exceptional condition, and
     * we found this marker on the marker stack while looking for a 
     * handler. Update the target node with the exception on top of 
     * the stack, and tell that we couldn't handle the exception, so 
     * unwinding continues.
     * 
     * (non-Javadoc)
     * @see bluejelly.runtime.Marker#signalRaise()
     */
    @Override
    protected boolean signalRaise() {
        this.target.update(new Raise(this.ctx.s[this.ctx.sp]));
        return false;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return target.toString();
    }
    
}
