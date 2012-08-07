package bluejelly;

import java.math.BigInteger;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.ann.JellyPrim;
import bluejelly.runtime.nodes.Str;

/**
 * Bag of {@link bluejelly.runtime.nodes.BigInt} primitives.
 * @author ppedemon
 */
// TODO Complete!
public class BigInt implements Module {

    // -------------------------------------------------------------------
    // BigInteger arithmetic
    // -------------------------------------------------------------------

    @JellyCode
    public void zero(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = new bluejelly.runtime.nodes.BigInt(BigInteger.ZERO);
    }

    @JellyCode
    public void one(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = new bluejelly.runtime.nodes.BigInt(BigInteger.ONE);
    }

    @JellyCode @JellyPrim(name="fromInt", arity=1)
    public void fromInt$1(ExecutionContext ctx) {
        String s = String.valueOf((bluejelly.runtime.nodes.Int)ctx.getWhnf(0));
        ctx.s[ctx.sp] = new bluejelly.runtime.nodes.BigInt(new BigInteger(s));
        ctx.slide(1,1);
    }

    @JellyCode @JellyPrim(name="fromStr", arity=1)
    public void fromStr$1(ExecutionContext ctx) {
        Str s = (Str)ctx.getWhnf(0);
        ctx.s[ctx.sp] = new bluejelly.runtime.nodes.BigInt(new BigInteger(s.s));
        ctx.slide(1,1);
        
    }
    
    @JellyCode @JellyPrim(name="add", arity=2)
    public void add$2(ExecutionContext ctx) {
        BigInteger x = ((bluejelly.runtime.nodes.BigInt)ctx.getWhnf(1)).i;
        BigInteger y = ((bluejelly.runtime.nodes.BigInt)ctx.getWhnf(0)).i;
        ctx.s[++ctx.sp] = new bluejelly.runtime.nodes.BigInt(x.add(y));
        ctx.slide(1,4);
    }

}
