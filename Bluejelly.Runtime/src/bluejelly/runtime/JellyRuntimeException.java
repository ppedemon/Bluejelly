package bluejelly.runtime;

/**
 * Class for BlueJelly runtime errors that might be ignored, 
 * letting the exception propagate to the top and aborting the
 * runtime with a suitable error msg and stack trace.
 * 
 * @author ppedemon
 * @see bluejelly.runtime.JellyException
 */
public class JellyRuntimeException extends RuntimeException {

    private static final long serialVersionUID = 5320923491329464413L;

    public JellyRuntimeException() {
    }

    public JellyRuntimeException(String message) {
        super(message);
    }

    public JellyRuntimeException(Throwable cause) {
        super(cause);
    }

    public JellyRuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

}
