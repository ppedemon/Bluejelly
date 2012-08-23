/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime;

import java.util.Arrays;

import bluejelly.runtime.nodes.App;
import bluejelly.runtime.nodes.Char;
import bluejelly.runtime.nodes.Code;
import bluejelly.runtime.nodes.Double;
import bluejelly.runtime.nodes.Executable;
import bluejelly.runtime.nodes.Int;
import bluejelly.runtime.nodes.NApp;
import bluejelly.runtime.nodes.Node;
import bluejelly.runtime.nodes.Raise;
import bluejelly.runtime.nodes.Str;
import bluejelly.runtime.nodes.TyCon;
import bluejelly.runtime.nodes.Updatable;


/**
 * A BlueJelly thread. This class provides the context for one
 * thread of execution in the runtime.
 * 
 * @author ppedemon
 */
public class ExecutionContext implements Runnable {
    
    // Execution context state describes what's on top of the stack
    private static enum State { REDEX, WHNF, PAP };
    
    // Node to evaluate
    private Node n;
    
    // Execution thread for this context
    private Thread t;
    
    // Future object for this context
    private Future<ExecutionContext> future;
    
    // Runtime
    private final Runtime runtime;
    
    // Context stack
    public int sp;
    public Node[] s;
    
    // Marker stack
    public int mp;
    public Marker[] m;

    // Base pointer
    public int bp;
    
    // Current state
    public volatile State state = State.REDEX;
    
    // Fast enter "register"
    private Executable jumpTo = null;
    
    // Registers for "return in registers" convention
    // tx = tag/int register
    // ax = arity register
    // dx = double register
    // sx = string register
    // 
    // Invariants:
    //  ax != -1 <==> Return in registers is taking place.
    //
    // We encode returned values as follows:
    // 
    // 1. Tycons: tx = tycon tag, ax = tycon arity.
    //      There are ax nodes on top of the stack, one 
    //      for each variable in the constructor.
    // 2. Ints: tx = unpacked int value, nothing on the stack
    // 3. Doubles: dx = unpacked double, nothing on the stack
    // 4. Strings: sx = unpacked string, nothing on the stack    
    public int tx;
    public int ax;
    public double dx;
    public String sx;
        
    /**
     * Construct a new instance using the given runtime.
     * @param runtime    runtime to be used by this context
     */
    public ExecutionContext(Runtime runtime) {
        this.runtime = runtime;
        this.sp = this.mp = -1;
        this.s = new Node[runtime.getConfig().initStackSize];
        this.m = new Marker[runtime.getConfig().initMarkerStackSize];
        this.ax = -1;
        this.tx = 0;
        this.dx = 0;
        this.sx = null;
    }
    
    /**
     * Evaluate the given node to normal form.
     * 
     * @param funId    function id
     * @param n        the node to evaluate
     * 
     * @return a {@link Future} object that can be used to examine
     * this context's final state when evaluation is done
     */
    public Future<ExecutionContext> eval(String funId, Node n) {
        this.n = n;
        this.future = new Future<ExecutionContext>();
        this.t = new Thread(this, funId);
        this.t.start();
        return this.future;
    }

    /**
     * Push the given {@link Marker} on top of the marker stack.
     * @param m    mark to push.
     */
    public void mPush(Marker m) {
        this.markerCheck(1);
        m.bp = this.bp;
        this.bp = this.sp - 1;
        this.m[++this.mp] = m;        
    }
    
    /**
     * Pop the topmost {@link Marker} from top of the marker stack.
     */
    public void mPop() {
        this.bp = this.m[this.mp].bp;
        this.m[this.mp--] = null;        
    }
    
    /**
     * Get executable node for the given function id.
     * 
     * @param funId    id for function whose code we are looking for
     * @return         intended {@link Code} node
     */
    public Executable getFun(String funId) {
        return this.runtime.getFun(funId);
    }    
    
    /**
     * Horrible hack to cope with stupid indirection updates: we must
     * be able to get the real updated value from a node.
     * 
     * @param off  
     *   offset from the top of the stack for the node whose 
     *   real updated value we want to retrieve
     *   
     * @return     real whnf value for the given node
     */
    public Node getWhnf(int off) {
        // Argh!
        Node n = this.s[this.sp - off];
        if (Updatable.class.isAssignableFrom(n.getClass())) {
            return ((Updatable)n).getInd();
        } else {
            return n;
        }
    }

    /**
     * Get whnf on top of the stack.
     * @return    whnf on top of the stack
     */
    public Node getWhnf() {
        return this.getWhnf(0);
    }
    
    /**
     * Check for <code>n</code> slot available in the node stack. Grow 
     * the stack if available space is not enough, or <code>raise</code> 
     * an exception if stack can't grow.
     * 
     * @param p    slots demanded for node stack
     */
    public void stackCheck(int n) {
        this.stackCheck(n, true);
    }

    /**
     * Debug support: dump the current stack frame, prefixed by some message.
     * @param msg    message used as prefix for stack frame dump
     */
    public void dumpStack(String msg) {
        Node[] frame = new Node[sp - bp];
        System.arraycopy(s, bp + 1, frame, 0, frame.length);
        //Node[] frame = new Node[sp + 1];
        //System.arraycopy(s, 0, frame, 0, frame.length);
        System.out.printf("{stack frame: %s:\n  %s}\n", 
            msg, Arrays.toString(frame));
    }
    
    /**
     * Check for <code>n</code> slot available in the marker stack. Grow 
     * the stack if available space is not enough, or <code>raise</code> 
     * an exception if stack can't grow.
     * 
     * @param p    slots demanded for marker stack
     */
    private void markerCheck(int n) {
        this.stackCheck(n, false);
    }
    
    /**
     * Check a stack for enough slots.
     * 
     * @param req requested slots
     * @param isNodeStack  whether we should check the node or the marker stack
     */
    private void stackCheck(int req, boolean isNodeStack) {
        Config c = this.runtime.getConfig();
        
        int len, ptr, max;
        if (isNodeStack) {
            len = s.length;
            ptr = this.sp;
            max = c.maxStackSize;
        } else {
            len = m.length;
            ptr = this.mp;
            max = c.maxMarkerStackSize;
        }

        if (len - ptr - 1 < req) {
            // newLen = first c.BSIZE multiple >= ptr + req + 1
            int targetLen = ptr + req + 1;
            int newLen = (targetLen + Config.BSIZE - 1) & -Config.BSIZE;
            newLen = Math.min(newLen, max);
            
            if (newLen > len) {
                if (isNodeStack) {
                    //System.out.printf("Asked for %d, so growing node stack: %d -> %d\n", req, len, newLen);
                    Node[] ns = new Node[newLen];
                    System.arraycopy(this.s, 0, ns, 0, ptr + 1);
                    this.s = ns;
                } else {
                    //System.out.printf("Asked for %d, so growing marker stack: %d -> %d\n", req, len, newLen);
                    Marker[] ms = new Marker[newLen];
                    System.arraycopy(this.m, 0, ms, 0, ptr + 1);
                    this.m = ms;                    
                }            
            }
            
            if (targetLen >= max - 1) {
                this.raiseStackOverflow();
            }
        }
    }
    
    /**
     * Check for <code>nargs</code> in the stack.
     * 
     * @param nargs    number of args we demand on the stack
     * @return         whether there are more than n args in th stack
     */
    public boolean argCheck(int nargs) {
        boolean inWhnf = false;
        
        if (this.sp - this.bp - 1 < nargs) {
            this.state = State.PAP;
            inWhnf = true;
        } else {
            this.s[this.sp--] = null;
        }
        
        return inWhnf;
    }
    
    /**
     * Notify this context that the node on top of the stack is in whnf.
     */
    public void whnf() {
        this.state = State.WHNF;
    }
    
    /**
     * Schedule a jump to a function, specified by name.
     * @param funId    id of the function to jump to
     */
    public void jump(String funId) {
        this.jumpTo = getFun(funId);
    }

    /**
     * Schedule a jump to a function specified by its code.
     * @param code    code of the function to jump to
     */
    public void jump(Executable code) {
        this.jumpTo = code;
    }

    /**
     * Variables to evaluate can be in normal form. In that case, it has
     * no point pushing the continuation just to discard it immediately.
     * So, we provide this method, that you should call when evaluating
     * a variable. If the variable is in normal form, just jump to the
     * continuation.
     * 
     * @param offset   offset of variable to evaluate, from the top of the stack
     * @param funId    continuation
     */
    public void evalVar(int off, String funId) {
        this.s[this.sp + 1] = this.s[this.sp - off];
        ++this.sp;
        Node n = this.s[this.sp];
        if (n instanceof Raise || n instanceof NApp || 
                (n instanceof Updatable && !((Updatable)n).isUpdated())) {
            pushCont(funId);
        } else {
            this.jump(funId);
        }
    }
    
    /**
     * Slide the stack.
     * 
     * @param off    offset where slide operation must begin
     * @param n      number of slots to slide out
     */
    public void slide(int off, int n) {
        int base = this.sp - off + 1;
        System.arraycopy(this.s, base, this.s,  base - n, off);
        Arrays.fill(this.s, this.sp - n + 1, this.sp + 1, null);
        this.sp -= n;
    }
    
    /**
     * Construct a not-updatable application node, and leave it
     * on top of the stack.
     * 
     * @param n    number of elements in the application node
     */
    public void mkNApp(int n) {
        Node[] nodes = this.collect(n);
        this.s[this.sp] = new NApp(nodes);
    }

    /**
     * Construct an updatable application node, and leave it
     * on top of the stack.
     * 
     * @param n    number of elements in the application node
     */
    public void mkApp(int n) {
        Node[] nodes = this.collect(n);
        this.s[this.sp] = new App(nodes);
    }

    /** 
     * Construct an empty non-updatable application node, and leave it
     * on top of the stack.
     * 
     * @param n    number of elements in the application node
     */
    public void allocNApp() {
        this.s[++this.sp] = new NApp();
    }
    
    /**
     * Construct an empty updatable application, and leave it
     * on top of the stack.
     * 
     * @param n    number of elements in the application node
     */
    public void allocApp() {
        this.s[++this.sp] = new App();
    }
    
    /**
     * Pop topmost n nodes and pack them into the {@link NApp} at offset off.
     * @param off    stack offset for napp node
     * @param n      number of nodes to pack.
     */
    public void packNApp(int off, int n) {
        Node[] nodes = this.collect(n);
        ((NApp)this.s[--this.sp - off]).pack(nodes);
    }

    /**
     * Pop topmost n nodes and pack them into the {@link App} at offset off.
     * @param off    stack offset for napp node
     * @param n      number of nodes to pack.
     */
    public void packApp(int off, int n) {
        Node[] nodes = this.collect(n);
        ((App)this.s[--this.sp - off]).pack(nodes);
    }

    /**
     * Put a type constructor on top of the stack.
     * @param tag    type constructor tag
     * @param n      number of nodes to pack from top of the stack
     */
    public void mkTyCon(int tag, int n) {
        if (n == 0) {
            this.s[++this.sp] = TyCon.mkzTyCon(tag);
        } else {
            Node[] nodes = this.collect(n);
            this.s[this.sp] = new TyCon(tag, nodes);
        }
    }
    
    /**
     * Allocate an empty type constructor on top of the stack.
     * @param tag    tag for the type constructor to allocate
     */
    public void allocTyCon(int tag) {
        /*
         * Note: we construct the tycon thru the constructor, hence
         * we assume that the constructor to allocate is not zero-ary.
         * This assumption turns out to be correct, because tycon allocs
         * are triggered from letrecs. Since zero-ary tycons don't depend
         * on other values, they can never be in a letrec.
         */
        this.s[++this.sp] = new TyCon(tag);
    }
    
    /**
     * Pack the topmost n nodes into the {@link TyCon} at offset off.
     * @param off    stack offset for the tycon
     * @param n      number of nodes to pack
     */
    public void packTyCon(int off, int n) {
        Node[] nodes = this.collect(n);
        ((TyCon)this.s[--this.sp - off]).pack(nodes);
    }
    
    /**
     * Push a continuation marker on top of the marker stack.
     * @param funId    id of function to jump to
     */
    public void pushCont(String funId) {
        this.mPush(new ContMarker(this, getFun(funId)));
    }
    
    /**
     * Return a type constructor without allocating it on the heap if possible.
     * This is our implementation of GHC's &quot;return in registers&quot; 
     * convention.
     * 
     * @param tag    tag of tycon to return
     * @param n      tycon's arity
     */
    public void retTyCon(int tag, int  n) {
        // Cool! we can return in registers, just set tx
        if (hasMatcher()) {
            this.tx = tag;
            this.ax = n;
            this.state = State.WHNF;
        } 
        // Oh well, let's allocate a tycon
        else {
            this.mkTyCon(tag, n);
        }
            
    }
    
    /**
     * Return in registers convention for ints.
     * @param i    int to return
     */
    public void retInt(int i) {
        if (hasMatcher()) {
            this.tx = i;
            this.ax = 0;
            this.state = State.WHNF;
        } else {
            this.s[++this.sp] = Int.mkInt(i);
        }
    }

    /**
     * Return in registers convention for chars.
     * @param c    character to return
     */
    public void retChar(char c) {
        if (hasMatcher()) {
            this.tx = c;
            this.ax = 0;
            this.state = State.WHNF;
        } else {
            this.s[++this.sp] = Char.mkChr(c);
        }
    }

    /**
     * Return in registers convention for doubles.
     * @param d    double to return
     */
    public void retDouble(double d) {
        if (hasMatcher()) {
            this.dx = d;
            this.ax = 0;
            this.state = State.WHNF;
        } else {
            this.s[++this.sp] = new Double(d);
        }        
    }

    /**
     * Return in registers convention for strings.
     * @param s    string to return
     */
    public void retStr(String s) {
        if (hasMatcher()) {
            this.sx = s;
            this.ax = 0;
            this.state = State.WHNF;
        } else {
            this.s[++this.sp] = new Str(s);
        }        
    }

    /**
     * Get tag of returned tycon. Might be in registers or on the stack.
     * @return    tag of recently returned tycon
     */
    public int getTag() {
        if (ax != -1) {
            return tx;
        } else {
            return ((TyCon)this.getWhnf()).getTag();
        }
    }

    /**
     * Get last returned int. Might be in registers or on the stack.
     * @return    last returned int
     */
    public int getInt() {
        if (ax != -1) {
            return tx;
        } else {
            return ((Int)this.getWhnf()).i;
        }
    }

    /**
     * Get last returned character. Might be in registers or on the stack.
     * @return    last returned character
     */
    public char getChar() {
        if (ax != -1) {
            return (char)tx;
        } else {
            return ((Char)this.getWhnf()).c;
        }
    }

    /**
     * Get last returned double. Might be in registers or on the stack.
     * @return    last returned double
     */
    public double getDouble() {
        if (ax != -1) {
            return dx;
        } else {
            return ((Double)this.getWhnf()).d;
        }
    }

    /**
     * Get last returned string. Might be in registers or on the stack.
     * @return    last returned string
     */
    public String getStr() {
        if (ax != -1) {
            return sx;
        } else {
            return ((Str)this.getWhnf()).s;
        }
    }

    /**
     * TyCon pattern matching support: code prologue for selected
     * alternative: clear ax register, flagging that the return in
     * convention is over. Put TyCon elems on top of the stack if
     * necessary.
     * 
     * Effect on the stack (worst case): adds {arity - 1} nodes on the stack
     */
    public void altTyConPrologue() {
        if (this.ax == -1) {
            TyCon tycon = (TyCon)this.getWhnf();
            Node[] nodes = tycon.getNodes();
            if (nodes.length > 1) {
                this.stackCheck(nodes.length - 1);
            }
            System.arraycopy(nodes, 0, this.s, this.sp, nodes.length);
            if (nodes.length == 0) {
                this.s[this.sp] = null;
            }
            this.sp += nodes.length - 1;
        } else {
            this.ax = -1;
        }
    }

    /**
     * Basic types pattern matching support: code prologue for selected
     * alternative: clear ax register.
     * 
     * Effect on the stack (worst case): leaves stack unchanged
     */
    public void altBasicPrologue() {
        if (this.ax == -1) {
            this.s[this.sp--] = null;
        } else {
            this.ax = -1;
        }
    }

    /**
     * Tycon pattern matching support: code prologue for default
     * alternative: pack the unpacked tycon on top of the stack.
     * 
     * Effect on the stack (worst case): adds one node (if ax == 0)
     */
    public void defTyConPrologue() {
        if (this.ax != -1) {
            if (this.ax == 0) { 
                this.stackCheck(1);
            }
            this.mkTyCon(this.tx, this.ax);
            this.ax = -1;
        }
    }

    /**
     * Int pattern matching support: code prologue for default
     * alternative: pack the unpacked int on top of the stack.
     * 
     * Effect on the stack (worst case): adds one node
     */
    public void defIntPrologue() {
        if (this.ax != -1) {
            this.stackCheck(1);
            this.s[++this.sp] = Int.mkInt(this.tx);
            this.ax = -1;
        }
    }

    /**
     * Char pattern matching support: code prologue for default
     * alternative: pack the unpacked character on top of the stack.
     * 
     * Effect on the stack (worst case): adds one node
     */
    public void defCharPrologue() {
        if (this.ax != -1) {
            this.stackCheck(1);
            this.s[++this.sp] = Char.mkChr((char)this.tx);
            this.ax = -1;
        }
    }

    /**
     * Double pattern matching support: code prologue for default
     * alternative: pack the unpacked double on top of the stack.
     * 
     * Effect on the stack (worst case): adds one node
     */
    public void defDoublePrologue() {
        if (this.ax != -1) {
            this.stackCheck(1);
            this.s[++this.sp] = new Double(this.dx);
            this.ax = -1;
        }
    }

    /**
     * String pattern matching support: code prologue for default
     * alternative: pack the unpacked string on top of the stack.
     * 
     * Effect on the stack (worst case): adds one node
     */
    public void defStrPrologue() {
        if (this.ax != -1) {
            this.stackCheck(1);
            this.s[++this.sp] = new Str(this.sx);
            this.ax = -1;
        }
    }

    /**
     * Raise the exception on top of the stack.
     */
    public void raise() {
        this.ax = -1;
        boolean hasHandler = false;
        while(this.mp >= 0 && !hasHandler) {
            Marker mrk = this.m[this.mp];
            this.mPop();
            hasHandler = mrk.signalRaise();
        }
        if (hasHandler) {
            int n = this.sp - this.bp - 2;
            if (n > 0)
                this.slide(2, n);
        } else {
            throw new JellyRuntimeException(this.s[this.sp].toString());
        }
    }
    
    
    /**
     * Register the node on top of the stack as an exception handler.
     */
    public void registerCatch() {
        Node n = this.s[this.sp];
        this.s[this.sp--] = null;
        this.mPush(new CatchMarker(this, n));
    }

    /**
     * Create an exception node. The runtime represents exceptions
     * as type constructors with the form:
     * 
     *   <code>data Exn = Exn String</code>
     *   
     * @param msg    exception message
     * @return       an exception node, just a tycon with a string node
     */
    public Node createException(String msg) {
        return new TyCon(0, new Node[]{new Str(msg)});
    }

    /**
     * Raise a blackhole exception. This happens when we try to 
     * enter a value we are computing, that is, when we find an
     * infinite loop.
     */
    public void raiseBlackhole() {
        this.stackCheck(1);
        this.s[++this.sp] = createException("Infinite loop");
        this.raise();
    }

    /**
     * Raise a stack overflow exception. This happens when we 
     * hit the maximum stack size on the node or marker stack.
     */
    public void raiseStackOverflow() {
        this.s[++this.sp] = createException("Stack overflow");
        this.raise();
    }

    /**
     * Raise a pattern match exception.
     */
    public void raisePatternMatchFail() {
        this.stackCheck(1);
        this.s[++this.sp] = createException("Pattern match failure");
        this.raise();
    }
    
    /**
     * Mini-interpreter evaluating the initial node for this context.
     */
    @Override
    public void run() {
        try {
            this.stackCheck(1);
            s[++sp] = this.n;
            bp = sp - 1;
            while (true) {
                while(this.state == State.REDEX) {
                    this.next();
                }
                if (this.mp >= 0) {
                    if (this.state == State.PAP)
                        this.m[this.mp].signalPap();
                    else
                        this.m[this.mp].signalWhnf();
                    this.mPop();
                    this.state = State.REDEX;
                } else {
                    break;
                }
            }
            
            this.future.set(this);
        } catch (Throwable t) {
            this.future.error(t);
        }
    }
    
    /**
     * Trigger next reduction step, if possible.
     */
    private void next() {
        // If node being reduced wants to jump to some code, do it
        if (this.jumpTo != null) {
            Executable code = this.jumpTo;
            this.jumpTo = null;
            code.fastEnter(this);
        }
        // Otherwise, just enter next redex
        else {
            s[sp].enter(this);
        }
    }
    
    /**
     * Is there a continuation marked as matcher on top of the stack?
     */
    private boolean hasMatcher() {
        if (this.mp >= 0) {
            Marker mkr = this.m[this.mp];
            return ContMarker.class == mkr.getClass() && 
                ((ContMarker)mkr).isMatcher();
        }
        return false;
    }
    
    /**
     * Collect <code>n</code> nodes from the top of the stack,
     * and remove them in the process. Stack pointer will point
     * to the first empty slot of the stack.
     * 
     * @param n    number of nodes to collect
     * @return     collected nodes, in stack order
     */
    protected Node[] collect(int n) {
        Node[] nodes = new Node[n];
        System.arraycopy(this.s, this.sp - n + 1, nodes, 0, n);
        Arrays.fill(this.s, this.sp - n + 1, this.sp + 1, null);
        this.sp -= n - 1;
        return nodes;
    }
        
}
