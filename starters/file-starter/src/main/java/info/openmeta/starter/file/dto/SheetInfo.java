package info.openmeta.starter.file.dto;

import info.openmeta.framework.orm.domain.FlexQuery;
import lombok.Data;

/**
 * The DTO of Excel sheet info.
 */
@Data
public class SheetInfo {
    private String modelName;

    private String sheetName;

    private FlexQuery flexQuery;
}
