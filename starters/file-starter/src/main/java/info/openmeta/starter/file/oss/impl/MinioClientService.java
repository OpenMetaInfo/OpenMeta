package info.openmeta.starter.file.oss.impl;

import info.openmeta.framework.base.exception.ExternalException;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.starter.file.constant.FileConstant;
import info.openmeta.starter.file.oss.OSSProperties;
import info.openmeta.starter.file.oss.OssClientService;
import io.minio.*;
import io.minio.errors.InternalException;
import io.minio.errors.MinioException;
import io.minio.errors.ServerException;
import io.minio.http.Method;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.InputStream;

/**
 * Minio OSS Client Service Implementation
 */
@Slf4j
@AllArgsConstructor
public class MinioClientService implements OssClientService {

    private MinioClient ossClient;

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
            ossClient.putObject(
                    PutObjectArgs.builder()
                            .bucket(getBucketName())
                            .object(ossKey)
                            .stream(inputStream, -1, 5 * 1024 * 1024) // 5MB part size
                            .build()
            );
            StatObjectResponse objectResult = ossClient.statObject(
                    StatObjectArgs.builder()
                            .bucket(getBucketName())
                            .object(ossKey)
                            .build()
            );
            return objectResult.etag();
        } catch (ServerException serverException) {
            throw new ExternalException("ServerException occurred while uploading the file {0}.\n{1}",
                    fileName, serverException.getMessage(), serverException);
        } catch (InternalException internalException) {
            throw new SystemException("ClientException occurred while uploading the file {0}.\n{1}",
                    fileName, internalException.getMessage(), internalException);
        } catch (MinioException minioException) {
            throw new SystemException("MinioException error occurred while uploading the file {0}.\n{1}",
                    fileName, minioException.getMessage(), minioException);
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
            return ossClient.getPresignedObjectUrl(
                    GetPresignedObjectUrlArgs.builder()
                            .method(Method.GET)
                            .bucket(getBucketName())
                            .object(ossKey)
                            .expiry(expirationInSeconds)
                            .build()
            );
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
            return ossClient.getObject(
                    GetObjectArgs.builder()
                            .bucket(getBucketName())
                            .object(ossKey)
                            .build()
            );
        } catch (ServerException serverException) {
            throw new ExternalException("ServerException occurred while downloading the file {0}.\n{1}",
                    fileName, serverException.getMessage(), serverException);
        } catch (InternalException internalException) {
            throw new SystemException("ClientException occurred while downloading the file {0}.\n{1}",
                    fileName, internalException.getMessage(), internalException);
        } catch (MinioException minioException) {
            throw new SystemException("MinioException error occurred while downloading the file {0}.\n{1}",
                    fileName, minioException.getMessage(), minioException);
        } catch (Exception e) {
            throw new SystemException("Unexpected error occurred while downloading the file {0}.\n{1}",
                    fileName, e.getMessage(), e);
        }
    }

}
