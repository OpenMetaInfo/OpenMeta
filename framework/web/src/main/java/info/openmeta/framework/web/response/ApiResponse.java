package info.openmeta.framework.web.response;

import info.openmeta.framework.base.enums.ResponseCode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * API response body:
 *    {
 *      "code": 2000,
 *      "message": "Success",
 *      "data": [{...}],
 *      "error": ""
 *    }
 * The code represents the business status code of the response.
 * The message is a brief description of the response.
 * The data is the business result of the API.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(name = "API Response Body")
public class ApiResponse<T> {

    @Schema(description = "Status Code")
    private Integer code;

    @Schema(description = "Common Message")
    private String message;

    @Schema(description = "Result Data")
    private T data;

    @Schema(description = "Error Message")
    private String error;

    /**
     * Generate a response body for the API, with the provided status code, message, and data.
     *
     * @param code response code
     * @param message message of the response.
     * @param data data of the response.
     * @return ApiResponse<T>
     * @param <T> The type of the data in the response.
     */
    public static <T> ApiResponse<T> result(Integer code, String message, T data) {
        return new ApiResponse<>(code, message, data, null);
    }

    /**
     * Generate exception response body, with the provided responseCode object and error message.
     *
     * @param responseCode response code object.
     * @param error error message of the response.
     * @return ApiResponse<T>
     */
    public static ApiResponse<String> exception(ResponseCode responseCode, String error) {
        Integer code = responseCode.getCode();
        String message = responseCode.getMessage();
        return new ApiResponse<>(code, message, null, error);
    }

    /**
     * Directly return success, using `ApiResponse.success()`.
     *
     * @return ApiResponse<T>
     */
    public static <T> ApiResponse<T> success() {
        Integer code = ResponseCode.SUCCESS.getCode();
        String message = ResponseCode.SUCCESS.getMessage();
        return new ApiResponse<>(code, message, null, null);
    }

    /**
     * Return success with data, using `ApiResponse.success(data)`.
     *
     * @param data data of the response.
     * @return ApiResponse<T>
     * @param <T> The type of the data in the response.
     */
    public static <T> ApiResponse<T> success(T data) {
        Integer code = ResponseCode.SUCCESS.getCode();
        String message = ResponseCode.SUCCESS.getMessage();
        return new ApiResponse<>(code, message, data, null);
    }

    /**
     * Return redirect response to trigger client-side redirection.
     *
     * @param url redirect url
     * @return redirect response
     */
    public static ApiResponse<String> redirect(String url) {
        Integer code = ResponseCode.REDIRECT.getCode();
        String message = ResponseCode.REDIRECT.getMessage();
        return new ApiResponse<>(code, message, url, null);
    }

}
