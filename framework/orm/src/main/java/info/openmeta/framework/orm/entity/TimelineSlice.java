package info.openmeta.framework.orm.entity;

import lombok.Data;

import java.io.Serializable;
import java.time.LocalDate;

/**
 * The minimal attributes of timeline model slice.
 */
@Data
public class TimelineSlice {
    private Serializable id;
    private Serializable sliceId;
    private LocalDate effectiveStartDate;
    private LocalDate effectiveEndDate;
}
