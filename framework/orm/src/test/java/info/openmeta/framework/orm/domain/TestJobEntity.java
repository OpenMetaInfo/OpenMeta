package info.openmeta.framework.orm.domain;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class TestJobEntity {
    private String title;
    private Integer value;
}
