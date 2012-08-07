/**
 * 
 */
package bluejelly;

import static bluejelly.Bools.FALSE;
import static bluejelly.Bools.TRUE;
import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.ann.JellyPrim;

/**
 * Bag of double primitives.
 * @author ppedemon
 */
public class Double implements Module {

	// -------------------------------------------------------------------
	// Integer arithmetic
	// -------------------------------------------------------------------

	@JellyCode @JellyPrim(name="add", arity=2)
	public void add$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retDouble(x+y);
	}
	
	@JellyCode @JellyPrim(name="sub", arity=2)
	public void sub$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retDouble(x-y);
	}
	
	@JellyCode @JellyPrim(name="mul", arity=2)
	public void mul$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retDouble(x*y);
	}

	@JellyCode @JellyPrim(name="div", arity=2)
	public void div$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retDouble(x/y);
	}

	@JellyCode @JellyPrim(name="rem", arity=2)
	public void rem$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retDouble(x%y);
	}

	@JellyCode @JellyPrim(name="neg", arity=1)
	public void neg$1(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,2);
		ctx.retDouble(-x);
	}

	// -------------------------------------------------------------------
	// Integer (in)equality
	// -------------------------------------------------------------------
	
	@JellyCode @JellyPrim(name="eq", arity=2)
	public void eq$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retTyCon(x==y?TRUE:FALSE,0);		
	}

	@JellyCode @JellyPrim(name="neq", arity=2)
	public void neq$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retTyCon(x!=y?TRUE:FALSE,0);		
	}

	@JellyCode @JellyPrim(name="lt", arity=2)
	public void lt$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retTyCon(x<y?TRUE:FALSE,0);		
	}

	@JellyCode @JellyPrim(name="gt", arity=2)
	public void gt$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retTyCon(x>y?TRUE:FALSE,0);		
	}

	@JellyCode @JellyPrim(name="leq", arity=2)
	public void leq$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retTyCon(x<=y?TRUE:FALSE,0);		
	}

	@JellyCode @JellyPrim(name="geq", arity=2)
	public void geq$2(ExecutionContext ctx) {
		double x = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(1)).d;
		double y = ((bluejelly.runtime.nodes.Double)ctx.getWhnf(0)).d;
		ctx.slide(0,4);
		ctx.retTyCon(x>=y?TRUE:FALSE,0);		
	}

}
