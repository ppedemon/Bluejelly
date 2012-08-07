package bluejelly.runtime;

/**
 * Abstract stack marker. Markers delimit stack frames, and know 
 * how to modify the runtime when found on the control stack.
 * 
 * @author ppedemon
 */
public abstract class Marker {

	// Execution context this marker refers to
	ExecutionContext ctx;
	
	// Old base pointer, ctx.sp[bp] = beginning of the prev stack frame
	protected int bp;
	
	/**
	 * Construct a new marker for the given context.
	 */
	public Marker(ExecutionContext ctx) {
		this.ctx = ctx;
	}
	
	/**
	 * Signal to this marker that a partial application was found on
	 * top of the stack. Whatever does the marker to the context is
	 * subclass-dependent.
	 */
	protected abstract void signalPap();

	/**
	 * Signal to this marker that a whnf was found on top of the stack.
	 * Whatever does the marker to the context is subclass-dependent.
	 */
	protected abstract void signalWhnf();
	
	/**
	 * Signal to this marker that the runtime is looking for a handler
	 * for the exceptional value on top of the stack.
	 * 
	 * @param ex    node denoting the exception currently thrown
	 * @return      whether this marker can handle the exception
	 */
	protected abstract boolean signalRaise();
}
