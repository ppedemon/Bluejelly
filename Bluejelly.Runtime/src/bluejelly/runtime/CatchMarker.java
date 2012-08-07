package bluejelly.runtime;

import bluejelly.runtime.nodes.Node;

/**
 * Exception handler marker.
 * 
 * @author ppedemon
 *
 */
public class CatchMarker extends Marker {

	// Expression to execute for error handling
	private final Node handler;
	
	/**
	 * Construct a new handler marker.
	 * @param ctx        context associated to this marker
	 * @param handler    expression to execute when handling an error
	 */
	public CatchMarker(ExecutionContext ctx, Node handler) {
		super(ctx);
		this.handler = handler;
	}
	
	/* (non-Javadoc)
	 * @see bluejelly.runtime.Marker#signalPap()
	 */
	@Override
	protected void signalPap() {
		// Don't care about partial applications, nothing to do

	}

	/* (non-Javadoc)
	 * @see bluejelly.runtime.Marker#signalWhnf()
	 */
	@Override
	protected void signalWhnf() {
		// Don't care about nodes in whnf, nothing to do
	}

	/*
	 * So, we finally found a catch marker! Execution must continue by 
	 * calling this.handler, with the exception on top of the stack as 
	 * parameter. Just push the handler, and return true (thus telling
	 * the context to stop unwinding).
	 * 
	 * (non-Javadoc)
	 * @see bluejelly.runtime.Marker#signalRaise()
	 */
	@Override
	protected boolean signalRaise() {
		this.ctx.stackCheck(1);
		this.ctx.s[++this.ctx.sp] = this.handler;
		return true;
	}
}
