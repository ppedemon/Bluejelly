package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.UpdMarker;

/**
 * Abstract updatable nodes. Subclasses of this node represent a
 * non-whnf redex that will be updated with its normal form the first
 * time it's entered. Subsequent enters will return the  normal form 
 * redex computed the first time.
 *  
 * @author ppedemon
 *
 */
public abstract class Updatable implements Node {

	// Pointer to node used to update this one
	private Node ind = null;
	
	// Blackhole flag
	private boolean blackholed = false;
	
	/**
	 * Get indirection node.
	 * @return    indirection node
	 */
	public Node getInd() {
		return this.ind;
	}
	
	/**
	 * Tell whether this node was updated.
	 * @return    whether this node as updated    
	 */
	public boolean isUpdated() {
		return this.ind != null;
	}
	
	/* (non-Javadoc)
	 * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
	 */
	@Override
	public void enter(ExecutionContext ctx) {
		if (this.blackholed) {
			ctx.raiseBlackhole();
		} else if (this.ind != null) {
			this.ind.enter(ctx);
		} else {
			this.blackholed = true;
			UpdMarker u = new UpdMarker(ctx, this);
			ctx.mPush(u);
			this.reduce(ctx);
		}
	}
	
	/**
	 * Update this node with some other.
	 * @param n    node this instance must be updated with
	 */
	public void update(Node n) {
		if (Updatable.class.isAssignableFrom(n.getClass())) {
			Updatable u = (Updatable)n;
			if (u.isUpdated()) {
				n = u.getInd();
			}
		}
		this.ind = n;
		this.blackholed = false;
	}
	
	/**
	 * This is the method you want to override for the real evaluation
	 * of an updatable node. After reduce took place, whnf for this node
	 * will be used for further evaluations.
	 * 
	 * @param ctx    {@link ExecutionContext} where this node is executing
	 */
	public abstract void reduce(ExecutionContext ctx);

}
