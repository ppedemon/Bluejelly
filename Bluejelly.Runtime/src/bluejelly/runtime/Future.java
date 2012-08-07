package bluejelly.runtime;

/**
 * a <code>Future</code> models the result of an asynchronous operation
 * returning a result with type <code>T</code>.
 * 
 * @author ppedemon
 */
public class Future<T> {
	
	private static final int NO_TIMEOUT = -1;
	
	// Mutually exclusive, invariant: t != null ^ e != null == true
	private T t = null;
	private Throwable e = null;
	
	/**
	 * Tell whether the task represented by this instance is done.
	 * 
	 * @return 
	 *   <code>true</code> if the task is done, <code>false</code> otherwise
	 */
	public synchronized boolean isDone() {
		return t != null || e != null;
	}
		
	/**
	 * Get the result of the task represented by this instance. This method
	 * will block for <code>timeout</code> msecs until a result is available.
	 * 
	 * @param timeout    
	 *   msecs to wait, wait forever if timeout = -1
	 * 
	 * @return           
	 *   task result, will be <code>null</code> if timeout expires
	 *     
	 * @throws JellyException
	 *   if task throws an exception or future is interrupted while waiting
	 */
	public synchronized T get(long timeout) throws JellyException {
		if (this.t != null) {
			return this.t;
		} else if (this.e != null) {
			if (this.e instanceof JellyException) {
				throw (JellyException)e;
			} else {
				throw new JellyException(this.e);
			}
		}

		try {
			if (timeout == NO_TIMEOUT) {
				this.wait();
			} else {
				this.wait(timeout);
			}
		} catch (InterruptedException e) {
			throw new JellyException(e);
		}
		
		return this.get(NO_TIMEOUT);
	}
	
	/**
	 * Get the result of the task represented by this instance. This method
	 * will block until a result is available.
	 * 
	 * @return
	 *   task result

	 * @throws JellyException
	 *   if task throws an exception or future is interrupted while waiting
	 */
	public T get() throws JellyException {
		return this.get(NO_TIMEOUT);
	}
	
	/**
	 * Invoked by the asynchronous task when it's over.
	 * @param t    task result
	 */
	protected synchronized void set(T t) {
		this.t = t;
		this.e = null;
		this.notify();
	}
	
	protected synchronized void error(Throwable e) {
		this.t = null;
		this.e = e;
		this.notify();
	}
}
