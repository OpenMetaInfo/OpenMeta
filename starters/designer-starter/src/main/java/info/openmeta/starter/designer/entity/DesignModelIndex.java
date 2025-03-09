package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * DesignModelIndex Model
 */
@Data
@Schema(name = "DesignModelIndex")
@EqualsAndHashCode(callSuper = true)
public class DesignModelIndex extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "External ID")
    private Long externalId;

    @Schema(description = "APP ID")
    private Long appId;

    @Schema(description = "Env ID")
    private Long envId;

    @Schema(description = "Index Title")
    private String name;

    @Schema(description = "Index Code")
    private String code;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Model ID")
    private Long modelId;

    @Schema(description = "Index Name")
    private String indexName;

    @Schema(description = "Index Fields")
    private List<String> indexFields;

    @Schema(description = "Is Unique Index")
    private Boolean uniqueIndex;

    @Schema(description = "Deleted")
    private Boolean deleted;
}