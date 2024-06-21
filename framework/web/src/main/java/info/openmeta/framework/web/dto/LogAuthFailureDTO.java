package info.openmeta.framework.web.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * Auth failure log DTO
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class LogAuthFailureDTO implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    public static final String MODEL_NAME = "LogAuthFailure";

    @Schema(description = "API path")
    private String apiPath;

    @Schema(description = "Failure reason")
    private String failureReason;

    @Schema(description = "Error stack")
    private String errorStack;
}