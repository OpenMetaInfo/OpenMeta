package info.openmeta.framework.base.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Response code enum
 */
@Getter
@AllArgsConstructor
public enum ResponseCode {

    REDIRECT(302, "Redirect"),

    SUCCESS(2000, "Success"),

    /** Client Exception */
    UNAUTHORIZED(4000, "Please login first!"),
    VERIFY_EXCEPTION(4010, "Verify exception"),
    TOKEN_EXPIRED(4011, "Token expired"),
    TOKEN_INVALID(4012, "Token invalid"),

    BUSINESS_EXCEPTION(4040, "Business exception"),
    PERMISSION_DENIED(4050, "Permission denied"),

    BAD_REQUEST(4060, "Request parameter error"),
    REQUEST_NOT_FOUND(4061, "Request resource not found."),
    HTTP_BAD_METHOD(4062, "Request method not supported."),

    VERSION_CHANGED(4070, "Data has been modified, please refresh and try again."),

    /** Server exception */
    ERROR(5000, "System exception, please feedback to the administrator."),
    BAD_SQL_STATEMENT(5010, "SQL Exception"),

    /** External exception */
    EXTERNAL_EXCEPTION(6000, "External system exception"),

    ;

    private final Integer code;
    private final String message;
}
