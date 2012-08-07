package bluejelly;

import static bluejelly.Bools.FALSE;
import static bluejelly.Bools.TRUE;
import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.ann.JellyPrim;

/**
 * Bag of int primitives.
 * @author ppedemon
 */
public class Int implements Module {

    // -------------------------------------------------------------------
    // Integer arithmetic
    // -------------------------------------------------------------------
    
    @JellyCode @JellyPrim(name="add", arity=2)
    public void add$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x+y);
    }
    
    @JellyCode @JellyPrim(name="sub", arity=2)
    public void sub$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x-y);
    }
    
    @JellyCode @JellyPrim(name="mul", arity=2)
    public void mul$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x*y);        
    }

    @JellyCode @JellyPrim(name="div", arity=2)
    public void div$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        if (y == 0) {
            ctx.s[++ctx.sp] = ctx.createException("Division by zero");
            ctx.raise();
        } else {
            ctx.slide(0,4);
            ctx.retInt(x/y);
        }
    }

    @JellyCode @JellyPrim(name="rem", arity=2)
    public void rem$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        if (y == 0) {
            ctx.s[++ctx.sp] = ctx.createException("Division by zero");
            ctx.raise();
        } else {
            ctx.slide(0,4);
            ctx.retInt(x%y);
        }
    }

    @JellyCode @JellyPrim(name="neg", arity=1)
    public void neg$1(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,2);
        ctx.retInt(-x);
    }

    @JellyCode @JellyPrim(name="not", arity=1)
    public void not$1(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,2);
        ctx.retInt(~x);
    }

    @JellyCode @JellyPrim(name="and", arity=2)
    public void and$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x&y);
    }

    @JellyCode @JellyPrim(name="or", arity=2)
    public void or$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x|y);
    }

    @JellyCode @JellyPrim(name="xor", arity=2)
    public void xor$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x^y);
    }

    @JellyCode @JellyPrim(name="shl", arity=2)
    public void shl$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x<<y);
    }

    @JellyCode @JellyPrim(name="shr", arity=2)
    public void shr$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x>>y);
    }

    @JellyCode @JellyPrim(name="lshr", arity=2)
    public void lshr$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retInt(x>>>y);
    }

    // -------------------------------------------------------------------
    // bluejelly.runtime.nodes.Integer (in)equality
    // -------------------------------------------------------------------
    
    @JellyCode @JellyPrim(name="eq", arity=2)
    public void eq$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retTyCon(x==y?TRUE:FALSE,0);        
    }

    @JellyCode @JellyPrim(name="neq", arity=2)
    public void neq$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retTyCon(x!=y?TRUE:FALSE,0);        
    }

    @JellyCode @JellyPrim(name="lt", arity=2)
    public void lt$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retTyCon(x<y?TRUE:FALSE,0);        
    }

    @JellyCode @JellyPrim(name="gt", arity=2)
    public void gt$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retTyCon(x>y?TRUE:FALSE,0);        
    }

    @JellyCode @JellyPrim(name="leq", arity=2)
    public void leq$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retTyCon(x<=y?TRUE:FALSE,0);        
    }

    @JellyCode @JellyPrim(name="geq", arity=2)
    public void geq$2(ExecutionContext ctx) {
        int x = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(1)).i;
        int y = ((bluejelly.runtime.nodes.Int)ctx.getWhnf(0)).i;
        ctx.slide(0,4);
        ctx.retTyCon(x>=y?TRUE:FALSE,0);        
    }

}
