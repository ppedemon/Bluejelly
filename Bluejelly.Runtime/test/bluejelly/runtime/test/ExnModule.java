package bluejelly.runtime.test;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.nodes.Int;
import bluejelly.runtime.nodes.Node;
import bluejelly.runtime.nodes.Str;

/**
 * Dummy exception testing module.
 * 
 * @author ppedemon
 */
public class ExnModule implements Module {
    
    @JellyCode
    public void raise(ExecutionContext ctx) {
        ctx.stackCheck(1);
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // Raise
        ctx.sp = sp;
        ctx.raise();
    }
    
    @JellyCode(arity=1)
    public void exnHandler(ExecutionContext ctx) {
        ctx.stackCheck(2);
        // PushInt 5
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(5);
        // PushCode primAddInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // Enter
        ctx.sp = sp;
        return;
    }
    
    @JellyCode
    public void handle(ExecutionContext ctx) {
        ctx.stackCheck(3);
        // PushCode exnHandler
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.ExnModule.exnHandler");
        // Catch
        ctx.sp = sp;
        ctx.registerCatch();
        // PushInt 0
        sp = ctx.sp;
        s[++sp] = Int.mkInt(0);
        // PushCode raise
        s[++sp] = ctx.getFun("bluejelly.runtime.test.ExnModule.raise");
        // PushCode primAddInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // MkApp 3
        ctx.sp = sp;
        ctx.mkApp(3);
        // PushVar 0
        sp = ctx.sp;
        s[sp+1] = s[sp]; ++sp;
        // PushVar 1
        s[sp+1] = s[sp-1]; ++sp;
        // Slide 2 1
        ctx.sp = sp;
        ctx.slide(2, 1);
        // EnterCode primAddInt
        ctx.sp = sp;
        ctx.jump("bluejelly.runtime.test.BazModule.primAddInt");
    }
    
    @JellyCode
    public void bottom(ExecutionContext ctx) {
        ctx.stackCheck(1);
        // PushCode bottom
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.ExnModule.bottom");
        // Enter
        ctx.sp = sp;
        return;
    }
    
    @JellyCode
    public void bottomCatched(ExecutionContext ctx) {
        ctx.stackCheck(2);
        // PushString "Ok"
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = new Str("Ok");
        // PushCode .const
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule._const");
        // MkNapp 2
        ctx.sp = sp;
        ctx.mkNApp(2);
        // Catch
        ctx.registerCatch();
        // PushCode bottom
        sp = ctx.sp;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.ExnModule.bottom");
        // Enter
        ctx.sp = sp;
        return;
    }
}
