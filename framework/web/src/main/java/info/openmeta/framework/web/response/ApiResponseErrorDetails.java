package info.openmeta.framework.web.response;

import info.openmeta.framework.base.enums.ResponseCode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * API response body with error details:
 *    {
 *      "code": 2000,
 *      "message": "Error",
 *      "data": [{...}],
 *      "error": "...error details..."
 *    }
 * The code represents the business status code of the response.
 * The message is a brief description of the response.
 * The data is the business result of the API.
 */
@Data
@Schema(name = "API Response Body")
@EqualsAndHashCode(callSuper = true)
public class ApiResponseErrorDetails<T> extends ApiResponse<T> {

    @Schema(description = "Error Details")
    private String error;

    private ApiResponseErrorDetails(Integer code, String message, T data, String error) {
        super(code, message, data);
        this.error = error;
    }

    /**
     * Generate exception response body, with the provided responseCode object and error message.
     *
     * @param responseCode response code object.
     * @param error error message of the response.
     * @return ApiResponse<T>
     */
    public static ApiResponseErrorDetails<String> exception(ResponseCode responseCode, String error) {
        Integer code = responseCode.getCode();
        String message = responseCode.getMessage();
        return new ApiResponseErrorDetails<>(code, message, null, error);
    }

    /**
     * Generate exception response body, with the provided responseCode object and error message.
     *
     * @param responseCode response code object.
     * @param data data of the response.
     * @param error error message of the response.
     * @return ApiResponse<T>
     * @param <T> data type.
     */
    public static <T> ApiResponseErrorDetails<T> exception(ResponseCode responseCode, T data, String error) {
        Integer code = responseCode.getCode();
        String message = responseCode.getMessage();
        return new ApiResponseErrorDetails<>(code, message, data, error);
    }
}
