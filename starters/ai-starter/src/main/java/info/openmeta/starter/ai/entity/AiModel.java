package info.openmeta.starter.ai.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.ai.enums.AiModelProvider;
import info.openmeta.starter.ai.enums.AiModelType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.math.BigDecimal;

/**
 * AiModel Model
 */
@Data
@Schema(name = "AiModel")
@EqualsAndHashCode(callSuper = true)
public class AiModel extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "Model Name")
    private String name;

    @Schema(description = "Model Code")
    private String code;

    @Schema(description = "Model Provider")
    private AiModelProvider modelProvider;

    @Schema(description = "Model Type")
    private AiModelType modelType;

    @Schema(description = "Input Price/1K tokens")
    private BigDecimal unitPriceInput;

    @Schema(description = "Output price/1K tokens")
    private BigDecimal unitPriceOutput;

    @Schema(description = "Max Context Tokens")
    private Integer maxTokens;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}