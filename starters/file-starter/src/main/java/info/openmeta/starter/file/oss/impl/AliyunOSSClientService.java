package info.openmeta.starter.file.oss.impl;

import com.aliyun.oss.ClientException;
import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSException;
import com.aliyun.oss.model.OSSObject;
import com.aliyun.oss.model.PutObjectResult;
import info.openmeta.framework.base.exception.ExternalException;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.starter.file.constant.FileConstant;
import info.openmeta.starter.file.oss.OSSProperties;
import info.openmeta.starter.file.oss.OssClientService;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.InputStream;
import java.net.URL;
import java.util.Date;

/**
 * Aliyun OSS Client Service Implementation
 */
@Slf4j
@AllArgsConstructor
public class AliyunOSSClientService implements OssClientService {

    private OSS ossClient;

    private OSSProperties ossProperties;

    private String getBucketName() {
        return ossProperties.getBucketName();
    }

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
        try {
            PutObjectResult objectResult = ossClient.putObject(getBucketName(), ossKey, inputStream);
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
     * Generates a pre-signed URL for the file
     *
     * @param ossKey             the key of the file stored in the OSS bucket
     * @param fileName           file name
     * @return the pre-signed URL
     */
    @Override
    public String getPreSignedUrl(String ossKey, String fileName) {
        int expirationInSeconds = ossProperties.getUrlExpireSeconds() == null ?
                FileConstant.DEFAULT_DOWNLOAD_URL_EXPIRE : ossProperties.getUrlExpireSeconds();
        return getPreSignedUrl(ossKey, expirationInSeconds, fileName);
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
    public String getPreSignedUrl(String ossKey, int expirationInSeconds, String fileName) {
        try {
            Date expiration = new Date(System.currentTimeMillis() + expirationInSeconds * 1000L);
            URL url = ossClient.generatePresignedUrl(getBucketName(), ossKey, expiration);
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
            OSSObject ossObject = ossClient.getObject(getBucketName(), ossKey);
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
