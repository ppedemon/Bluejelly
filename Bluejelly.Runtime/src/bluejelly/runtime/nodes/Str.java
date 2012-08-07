package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * A string node.
 * 
 * @author ppedemon
 */
public class Str implements Node {

	public final String s;
	
	/**
	 * Construct a new string node with the given value.
	 * @param s    value for this string node
	 */
	public Str(String s) {
		this.s = s;
	}
	
	/* (non-Javadoc)
	 * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
	 */
	@Override
	public void enter(ExecutionContext ctx) {
		ctx.whnf();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.s;
	}

}
