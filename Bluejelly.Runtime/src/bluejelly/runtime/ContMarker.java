package bluejelly.runtime;

import bluejelly.runtime.nodes.Executable;
import bluejelly.runtime.nodes.NApp;
import bluejelly.runtime.nodes.Node;

/**
 * Continuation marker. Whenever found on the stack, it will jump
 * to the function associated to this continuation.
 * 
 * @author ppedemon
 */
public class ContMarker extends Marker {

	// Continuation
	private Executable cont;
	
	/**
	 * Construct a continuation marker for the given {@link ExecutionContext}
	 * @param ctx     associated context
	 * @param cont    continuation code {@link Node}
	 */
	public ContMarker(ExecutionContext ctx, Executable cont) {
		super(ctx);
		this.cont = cont;
	}

	/**
	 * Is the code for this continuation a tycon matcher?
	 * @return    whether the code for this continuation is a tycon matcher
	 */
	public boolean isMatcher() {
		return cont.isMatcher();
	}
	
	/* 
	 * So, an evaluation routine finished with a partial application. 
	 * We must pack the whole stack frame on a NApp node, and schedule 
	 * a jump to the continuation.
	 * 
	 * (non-Javadoc)
	 * @see bluejelly.runtime.Marker#signalPap()
	 */
	@Override
	protected void signalPap() {
		int n = this.ctx.sp - this.ctx.bp;
		Node[] nodes = this.ctx.collect(n);
		NApp napp = new NApp(nodes);
		this.ctx.s[this.ctx.sp] = napp;
		
		//System.out.println("Evaluated to: " + napp + ", jumping to: " + this.funId);
		
		this.ctx.jump(this.cont);
	}

	/* 
	 * So, the execution context reached normal form. Just schedule
	 * a jump to the continuation.
	 * 
	 * (non-Javadoc)
	 * @see bluejelly.runtime.Marker#signalWhnf()
	 */
	@Override
	protected void signalWhnf() {
		//System.out.println("Evaluated to: " + ctx.getWhnf() + ", jumping to: " + this.funId);
		
		this.ctx.jump(this.cont);
	}

	/*
	 * So, we are unwinding the marker stack looking for a handler, and 
	 * we found a continuation. Nothing to do for continuations, so let's
	 * keep unwinding the stack.
	 * 
	 * (non-Javadoc)
	 * @see bluejelly.runtime.Marker#signalRaise()
	 */
	@Override
	protected boolean signalRaise() {
		return false;
	}

}
