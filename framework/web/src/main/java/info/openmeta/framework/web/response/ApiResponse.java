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
 *      "data": [{...}]
 *    }
 * The code represents the business status code of the response.
 * The message is a brief description of the response, or the title of warn and error popup.
 * The data is the business result of the API.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(name = "API Response Body")
public class ApiResponse<T> {

    @Schema(description = "Status Code")
    private Integer code;

    @Schema(description = "Title Message")
    private String message;

    @Schema(description = "Result Data")
    private T data;

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
        return new ApiResponse<>(code, message, data);
    }

    /**
     * Generate a response body, with the provided responseCode object and data.
     *
     * @param responseCode response code object.
     * @param data data of the response.
     * @return ApiResponse<T>
     * @param <T> The type of the data in the response.
     */
    public static <T> ApiResponse<T> result(ResponseCode responseCode, T data) {
        return result(responseCode.getCode(), responseCode.getMessage(), data);
    }

    /**
     * Generate a response body, with the provided responseCode object.
     *
     * @param responseCode response code object.
     * @return ApiResponse<T>
     * @param <T> The type of the data in the response.
     */
    public static <T> ApiResponse<T> result(ResponseCode responseCode) {
        return result(responseCode.getCode(), responseCode.getMessage(), null);
    }

    /**
     * Directly return success, using `ApiResponse.success()`.
     *
     * @return ApiResponse<T>
     */
    public static <T> ApiResponse<T> success() {
        return result(ResponseCode.SUCCESS);
    }

    /**
     * Return success with data, using `ApiResponse.success(data)`.
     *
     * @param data data of the response.
     * @return ApiResponse<T>
     * @param <T> The type of the data in the response.
     */
    public static <T> ApiResponse<T> success(T data) {
        return result(ResponseCode.SUCCESS.getCode(), ResponseCode.SUCCESS.getMessage(), data);
    }

}
