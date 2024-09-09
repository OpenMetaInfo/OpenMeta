package info.openmeta.starter.file.oss.impl;

import com.aliyun.oss.ClientException;
import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSException;
import com.aliyun.oss.model.OSSObject;
import com.aliyun.oss.model.PutObjectResult;
import info.openmeta.framework.base.exception.ExternalException;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.starter.file.oss.OssClientService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.Date;

/**
 * OSS Client Service Implementation
 */
@Slf4j
@Service
public class OssClientServiceImpl implements OssClientService {

    @Autowired
    private OSS ossClient;

    @Value("${aliyun.bucketName:}")
    private String bucketName;

    /**
     * Uploads the file stream to the OSS bucket
     *
     * @param ossKey   the key to store the file in the OSS bucket
     * @param inputStream the input stream of the file
     * @param fileName file name
     * @return the checksum of the uploaded file
     */
    @Override
    public String uploadStreamToOSS(String ossKey, InputStream inputStream, String fileName) {
        try (InputStream stream = inputStream) {
            PutObjectResult objectResult = ossClient.putObject(bucketName, ossKey, stream);
            return objectResult.getETag();
        } catch (OSSException ossException) {
            throw new ExternalException("OSSException occurred while uploading the file {0}.\n{1}",
                    fileName, ossException.getErrorMessage(), ossException);
        } catch (ClientException clientException) {
            throw new SystemException("ClientException occurred while uploading the file {0}.\n{1}",
                    fileName, clientException.getMessage(), clientException);
        } catch (Exception e) {
            throw new SystemException("Unexpected error occurred while uploading the file {0}.\n{1}",
                    fileName, e.getMessage(), e);
        }
    }

    /**
     * Uploads the file bytes to the OSS bucket
     *
     * @param ossKey   the key to store the file in the OSS bucket
     * @param data     the byte array of the file
     * @param fileName file name
     * @return the checksum of the uploaded file
     */
    @Override
    public String uploadByteToOSS(String ossKey, byte[] data, String fileName) {
        InputStream inputStream = new ByteArrayInputStream(data);
        return uploadStreamToOSS(ossKey, inputStream, fileName);
    }

    /**
     * Generates a pre-signed URL for the file
     *
     * @param ossKey             the key of the file stored in the OSS bucket
     * @param expirationInSeconds the duration for which the URL is valid
     * @param fileName           file name
     * @return the pre-signed URL
     */
    @Override
    public String getPreSignedUrl(String ossKey, long expirationInSeconds, String fileName) {
        try {
            Date expiration = new Date(System.currentTimeMillis() + expirationInSeconds * 1000);
            URL url = ossClient.generatePresignedUrl(bucketName, ossKey, expiration);
            return url.toString();
        } catch (Exception e) {
            throw new ExternalException("Error while generating the file URL {0}", fileName, e);
        }
    }

    /**
     * Downloads the file from the OSS bucket and returns the InputStream
     *
     * @param ossKey the key of the file stored in the OSS bucket
     * @return the InputStream of the file
     */
    @Override
    public InputStream downloadStreamFromOSS(String ossKey, String fileName) {
        try {
            OSSObject ossObject = ossClient.getObject(bucketName, ossKey);
            return ossObject.getObjectContent();
        } catch (OSSException ossException) {
            throw new ExternalException("OSSException occurred while downloading the file {0}.\n{1}",
                    fileName, ossException.getErrorMessage(), ossException);
        } catch (ClientException clientException) {
            throw new SystemException("ClientException occurred while downloading the file {0}.\n{1}",
                    fileName, clientException.getMessage(), clientException);
        } catch (Exception e) {
            throw new SystemException("Unexpected error occurred while uploading the file {0}.\n{1}",
                    fileName, e.getMessage(), e);
        }
    }

}
