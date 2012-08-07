package bluejelly.runtime;

/**
 * Class for BlueJelly runtime errors that 
 * must be handled properly in runtime code.
 * 
 * @author ppedemon
 * @see com.ibm.bjelly.runtime.JellyRuntimeException
 */
public class JellyException extends Exception {

	private static final long serialVersionUID = 8967778791077970623L;

	public JellyException() {
	}

	public JellyException(String message) {
		super(message);
	}

	public JellyException(Throwable cause) {
		super(cause);
	}

	public JellyException(String message, Throwable cause) {
		super(message, cause);
	}

}
