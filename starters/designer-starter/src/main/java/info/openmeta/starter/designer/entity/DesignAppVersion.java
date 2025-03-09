package info.openmeta.starter.designer.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.time.LocalDateTime;

/**
 * DesignAppVersion Model
 */
@Data
@Schema(name = "DesignAppVersion")
@EqualsAndHashCode(callSuper = true)
public class DesignAppVersion extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Env ID")
    private Long envId;

    @Schema(description = "Version Name")
    private String name;

    @Schema(description = "Upgrade description")
    private String description;

    @Schema(description = "Table DDL")
    private String ddlTable;

    @Schema(description = "Index DDL")
    private String ddlIndex;

    @Schema(description = "Versioned Content")
    private JsonNode versionedContent;

    @Schema(description = "Last Versioned Time")
    private LocalDateTime lastVersionedTime;

    @Schema(description = "Published")
    private Boolean published;

    @Schema(description = "Last Publish Time")
    private LocalDateTime lastPublishTime;

    @Schema(description = "Locked")
    private Boolean locked;

    @Schema(description = "Deleted")
    private Boolean deleted;
}