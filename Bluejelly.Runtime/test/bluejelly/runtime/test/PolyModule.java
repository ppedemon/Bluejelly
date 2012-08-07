package bluejelly.runtime.test;

import bluejelly.runtime.Dictionary;
import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.ann.JellyDict;
import bluejelly.runtime.nodes.Double;
import bluejelly.runtime.nodes.Int;
import bluejelly.runtime.nodes.Node;

/**
 * Dummy module for checking dictionary usage.
 * @author ppedemon
 */
public class PolyModule implements Module {
    
    @JellyDict
    public Dictionary fd = new Dictionary(null, 
            new String[] {"bluejelly.runtime.test.PolyModule.fFlt"}, null);

    @JellyDict
    public Dictionary id = new Dictionary(null, 
            new String[] {"bluejelly.runtime.test.PolyModule.fInt"}, null);

    @JellyCode(arity=1)
    public void fFlt(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.slide(0,1);
        ctx.retDouble(Math.PI);
    }

    @JellyCode(arity=1)
    public void fInt(ExecutionContext ctx) {
        ctx.stackCheck(1);
        Node[] s = ctx.s;
        int sp = ctx.sp;
        
        // PushInt 1
        s[++sp] = Int.mkInt(1);
        
        // EnterCode primAddInt
        ctx.sp = sp;
        ctx.jump("bluejelly.runtime.test.BazModule.primAddInt");
    }

    @JellyCode
    public void callFlt(ExecutionContext ctx) {
        ctx.stackCheck(3);
        Node[] s = ctx.s;
        int sp = ctx.sp;
        
        // PushFloat .0
        s[++sp] = new Double(0);
        // PushDict org.bluejelly.test.runtime.PolyModule.fd
        s[++sp] = ctx.getDict("bluejelly.runtime.test.PolyModule.fd");
        // PushMethod 0
        ctx.sp = sp;
        ctx.getMethod(0);
        // Enter
        return;
    }
    
    /*
     * NB: Technically, exactly the same as callFlt. Just using the
     * int dictionary "id", and passing an integer instead of a float.
     */
    @JellyCode
    public void callInt(ExecutionContext ctx) {
        ctx.stackCheck(3);
        Node[] s = ctx.s;
        int sp = ctx.sp;

        // PushInt 41
        s[++sp] = Int.mkInt(41);
        // PushDict org.bluejelly.test.runtime.PolyModule.id
        s[++sp] = ctx.getDict("bluejelly.runtime.test.PolyModule.id");
        // PushMethod 0
        ctx.sp = sp;
        ctx.getMethod(0);
        // Enter
        return;
    }
    
}
