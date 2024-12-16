package info.openmeta.starter.designer.upgrade.impl;

import info.openmeta.framework.base.enums.ResponseCode;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.URLUtils;
import info.openmeta.framework.web.dto.MetadataUpgradePackage;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import info.openmeta.starter.designer.upgrade.RemoteApiClient;
import info.openmeta.starter.metadata.constant.MetadataConstant;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Objects;

/**
 */
@Service
public class RemoteApiClientImpl implements RemoteApiClient {

    @Autowired
    private RestTemplate restTemplate;

    /**
     * Send a POST request via restTemplate
     * @param url Request URL
     * @param requestEntity Request body
     * @return Response
     */
    private <T> Object invokeRestTemplate(String url, HttpEntity<T> requestEntity) {
        // Specify the response type as APIResponse<Object>
        ParameterizedTypeReference<ApiResponse<Object>> responseType = new ParameterizedTypeReference<>() {};
        // Send the request
        ResponseEntity<ApiResponse<Object>> responseEntity = restTemplate.exchange(url, HttpMethod.POST, requestEntity, responseType);
        ApiResponse<Object> apiResponse = responseEntity.getBody();
        Assert.isTrue(HttpStatus.OK.equals(responseEntity.getStatusCode()),
                "Error occurred while calling remote API: {0}: {1} \n Request body: {2}",
                url, apiResponse, requestEntity.getBody());
        Assert.isTrue(apiResponse == null || ResponseCode.SUCCESS.getCode().equals(apiResponse.getCode()),
                "Error occurred while calling remote API: {0}: {1}\n Request body: {2}",
                url, apiResponse, requestEntity.getBody());
        return Objects.requireNonNull(apiResponse).getData();
    }

    /**
     * Call remote Rest API
     * @param accessToken Authorization token
     * @param url Full API URL
     * @param body Request body
     * @return ApiResponse
     */
    private <T> Object callRemoteApi(String accessToken, String url, T body) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(accessToken);
        HttpEntity<T> requestEntity = new HttpEntity<>(body, headers);
        return invokeRestTemplate(url, requestEntity);
    }

    /**
     * Call the remote upgrade API
     * @param appEnv App environment
     * @param modelPackages List of runtime model data packages to be upgraded
     */
    @Override
    public void remoteUpgrade(DesignAppEnv appEnv, List<MetadataUpgradePackage> modelPackages) {
        String url = URLUtils.buildUrl(appEnv.getUpgradeEndpoint(), MetadataConstant.METADATA_UPGRADE_API);
        // Get authorization token
        String accessToken = appEnv.getClientSecret();
        this.callRemoteApi(accessToken, url, modelPackages);
    }
}