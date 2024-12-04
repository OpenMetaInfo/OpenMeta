package info.openmeta.starter.file.constant;

/**
 * Constants for file processing.
 */
public interface FileConstant {
    // Label for the failed data.
    String FAILED_DATA = "Failed Data";

    // Column name for the failed reason.
    String FAILED_REASON = "Failed Reason";

    // The default value of the download URL expiration time in seconds, which is 300 seconds (5 minutes).
    int DEFAULT_DOWNLOAD_URL_EXPIRE = 300;
}
