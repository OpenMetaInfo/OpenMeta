package info.openmeta.starter.file.dto;

import info.openmeta.framework.orm.domain.Filters;
import lombok.Data;

/**
 * @author huangjinghao
 * @create 2024-09-13 19:19
 */
@Data
public class ExportTemplateDTO {

    private Long templateId;

    private Filters filters;

}
