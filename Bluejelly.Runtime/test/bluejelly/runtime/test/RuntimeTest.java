/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime.test;

import java.math.BigInteger;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Future;
import bluejelly.runtime.IdUtils;
import bluejelly.runtime.JellyException;
import bluejelly.runtime.JellyRuntimeException;
import bluejelly.runtime.Runtime;
import bluejelly.runtime.nodes.App;
import bluejelly.runtime.nodes.BigInt;
import bluejelly.runtime.nodes.Code;
import bluejelly.runtime.nodes.Double;
import bluejelly.runtime.nodes.Int;
import bluejelly.runtime.nodes.Str;
import bluejelly.runtime.nodes.TyCon;

/**
 * TestEnv runtime basic instructions.
 * @author ppedemon
 */
public class RuntimeTest extends TestCase {

    private Runtime runtime;
    
    private String foo;
    private String bar;
    private String baz;
    private String exn;
    private String poly;
    private String prn;
    
    @Override
    @Before
    protected void setUp() throws Exception {
        this.foo = FooModule.class.getName();
        this.bar = BarModule.class.getName();
        this.baz = BazModule.class.getName();
        this.exn = ExnModule.class.getName();
        this.poly = PolyModule.class.getName();
        this.prn = PrinterModule.class.getName();        
        this.runtime = new Runtime();
    }

    @Test
    public void testInt() throws JellyException {
        Future<ExecutionContext> f = this.runtime.eval("test", Int.mkInt(0));
        ExecutionContext ctx = f.get();
        assertEquals(0, ctx.sp);
        assertEquals(((Int)ctx.s[0]).i, 0);
    }
    
    @Test
    public void testTooFewArgs() throws JellyException {
        String funId = IdUtils.qualify(this.bar, "whnfTest");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(ctx.sp, 1);
        assertEquals(((Int)ctx.s[0]).i, 1);
        assertEquals(((Code)ctx.s[1]).getFunctionName(), "_const");
    }
    
    @Test
    public void testMain() throws JellyException {
        String funId = IdUtils.qualify(this.foo, "main");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(ctx.sp, 0);
        assertEquals(((Code)ctx.s[0]).getFunctionName(), "id");        
    }
    
    @Test
    public void testTest() throws JellyException {
        String funId = IdUtils.qualify(this.foo, "test");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(ctx.sp, 0);
        assertEquals(((Int)ctx.s[0]).i, 43);                
    }
    
    @Test
    public void testNApp() throws JellyException {
        String funId = IdUtils.qualify(this.foo, "nApp");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(ctx.sp, 0);
        assertEquals(((Int)ctx.s[0]).i, 2);                        
    }
    
    @Test
    public void testApp() throws JellyException {
        String funId = IdUtils.qualify(this.foo, "app");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(ctx.sp, 1);
        assertEquals(((Int)ctx.s[0]).i, 1);    
        assertEquals(((Code)ctx.s[1]).getFunctionName(), "swap");
    }

    @Test
    public void testDummyLetrec() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "dummyLetrec");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(0, ((Int)ctx.s[0]).i);
    }
    
    @Test
    public void testDummyTyCon() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "dummyTyCon");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(0, ((Int)ctx.s[0]).i);
    }
    
    @Test
    public void testPapUpd() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "papUpd");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(1, ((Int)ctx.s[0]).i);
    }
    
    @Test
    public void testWhnfUpd() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "whnfUpd");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(0, ((TyCon)ctx.s[0]).getTag());
        assertTrue(App.class.isAssignableFrom(((TyCon)ctx.s[0]).getNodes()[0].getClass()));
    }
    
    @Test
    public void testAddInt() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "addInt");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(3, ((Int)ctx.s[0]).i);
    }
    
    @Test
    public void testUpdEval() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "updEval");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(12, ((Int)ctx.getWhnf(0)).i);
    }
    
    @Test
    public void testCaf() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "caf");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(6, ((Int)ctx.getWhnf(0)).i);
    }
    
    @Test
    public void testQuickSum() throws JellyException {
        String funId = IdUtils.qualify(this.baz, "quickSum");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(21,((Int)ctx.getWhnf()).i);        
    }
    
    @Test
    public void testIntMatch() throws JellyException {
        String funId = IdUtils.qualify(this.bar, "intMatch");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(4, ((Int)ctx.getWhnf(0)).i);
    }
    
    @Test
    public void testTyCon1() throws JellyException {
        String funId = IdUtils.qualify(this.bar, "tycon1");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(3, ((Int)ctx.getWhnf(0)).i);
    }
    
    @Test
    public void testTyCon2() throws JellyException {
        String funId = IdUtils.qualify(this.bar, "tycon2");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(-1, ((Int)ctx.getWhnf(0)).i);
    }

    @Test
    public void testTrickyMatch() throws JellyException {
        String funId = IdUtils.qualify(this.bar, "trickyMatch");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        TyCon t = (TyCon)ctx.getWhnf();
        assertEquals(0, t.getTag());
        assertEquals(1, ((Int)t.getNodes()[0]).i);
        assertEquals(2, ((Int)t.getNodes()[1]).i);
    }

    @Test
    public void testTrickyMatch1() throws JellyException {
        String funId = IdUtils.qualify(this.bar, "trickyMatch1");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(-1, ((Int)ctx.getWhnf()).i);
    }

    @Test
    public void testRaise() {
        String funId = IdUtils.qualify(this.exn, "raise");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        try {
            // f.get() must raise an exception
            f.get();
            assertTrue(false);
        } catch (JellyException e) {
            // We expect to get here
            assertTrue(e.getCause() instanceof JellyRuntimeException);
        }
    }
    
    @Test
    public void testHandle() throws JellyException {
        String funId = IdUtils.qualify(this.exn, "handle");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(6, ((Int)ctx.getWhnf()).i);
    }
    
    @Test
    public void testBottom() {
        try {
            String funId = IdUtils.qualify(this.exn, "bottom");
            Future<ExecutionContext> f = this.runtime.eval(funId);
            f.get();
            assertTrue(false);
        } catch (JellyException e) {
            assertTrue(e.getCause() instanceof JellyRuntimeException);
        }
    }
    
    @Test
    public void testBottomCatched() throws JellyException {
        String funId = IdUtils.qualify(this.exn, "bottomCatched");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals("Ok",((Str)ctx.getWhnf()).s);
    }
        
    @Test
    public void testPolyDbl() throws JellyException {
        String funId = IdUtils.qualify(this.poly, "polyDbl");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(Math.PI,((Double)ctx.getWhnf()).d);
    }
    
    @Test
    public void testPolyInt() throws JellyException {
        String funId = IdUtils.qualify(this.poly, "polyInt");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        ExecutionContext ctx = f.get();
        assertEquals(42,((Int)ctx.getWhnf()).i);
    }

    @Test
    public void testPrintInt() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printWhnf");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }
    
    @Test
    public void testPrintSimpleTyCon() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printSimpleTyCon");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }
    
    @Test
    public void testPrintComplexTyCon() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printComplexTyCon");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }

    @Test
    public void testPrintEmptyList() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printEmptyList");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }

    @Test
    public void testPrintNonEmptyList() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printNonEmptyList");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }

    /*
    @Test
    public void testPrintOnes() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printOnes");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }
    */

    /*
    @Test
    public void testPrintCyclic() throws JellyException {
        String funId = IdUtils.qualify(this.prn, "printCyclic");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        f.get();
    }
    */

    @Test
    public void testBigIntArith() throws JellyException {
        String funId = IdUtils.qualify(this.foo, "bigIntArith");
        Future<ExecutionContext> f = this.runtime.eval(funId);
        assertTrue(((BigInt)f.get().getWhnf(0)).i
                .equals(new BigInteger("335385764583214543694543533")));
    }

}
