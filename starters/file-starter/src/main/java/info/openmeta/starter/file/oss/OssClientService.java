package info.openmeta.starter.file.oss;

import java.io.InputStream;

public interface OssClientService {

    /**
     * Uploads the file stream to the OSS bucket
     *
     * @param ossKey   the key to store the file in the OSS bucket
     * @param inputStream the input stream of the file
     * @param fileName file name
     * @return the checksum of the uploaded file
     */
    String uploadStreamToOSS(String ossKey, InputStream inputStream, String fileName);

    /**
     * Uploads the file bytes to the OSS bucket
     *
     * @param ossKey   the key to store the file in the OSS bucket
     * @param data     the byte array of the file
     * @param fileName file name
     * @return the checksum of the uploaded file
     */
    String uploadByteToOSS(String ossKey, byte[] data, String fileName);

    /**
     * Generates a pre-signed URL for the file
     *
     * @param ossKey             the key of the file stored in the OSS bucket
     * @param expirationInSeconds the duration for which the URL is valid
     * @param fileName           file name
     * @return the pre-signed URL
     */
    String getPreSignedUrl(String ossKey, long expirationInSeconds, String fileName);

    /**
     * Downloads the file from the OSS bucket and returns the InputStream
     *
     * @param ossKey the key of the file stored in the OSS bucket
     * @return the InputStream of the file
     */
    InputStream downloadStreamFromOSS(String ossKey, String fileName);
}
