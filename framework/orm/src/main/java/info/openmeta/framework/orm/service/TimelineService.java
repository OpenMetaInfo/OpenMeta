package info.openmeta.framework.orm.service;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.entity.TimelineSlice;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Timeline Model Service
 * Specific processing methods for the timeline model.
 */
public interface TimelineService {

    /**
     * Append `effectiveDate` filters when query the timeline model: `effectiveStartDate <= effectiveDate <= effectiveEndDate`.
     * If the original filters already contain `effectiveStartDate` or `effectiveEndDate`, the `originalFilters` will
     * be returned directly, which means the query is across time periods.
     *
     * @param modelName model name
     * @param originalFilters original filters
     * @return updated filters
     */
    Filters appendTimelineFilters(String modelName, Filters originalFilters);

    /**
     * Append `effectiveDate` filters when query the timeline model: `effectiveStartDate <= effectiveDate <= effectiveEndDate`.
     * If `isAcrossTimeline = true`, the `originalFilters` will be returned directly,
     * which means the query is across time periods.
     *
     * @param modelName model name
     * @param flexQuery flexQuery object
     * @return updated filters
     */
    Filters appendTimelineFilters(String modelName, FlexQuery flexQuery);

    /**
     * Get the TimelineSlice object by `sliceId`.
     *
     * @param modelName model name
     * @param sliceId the sliceId of timeline model
     * @return TimelineSlice object
     */
    TimelineSlice getTimelineSlice(String modelName, Serializable sliceId);

    /**
     * Create multiple slices of timeline model, and return the data list with `sliceId`.
     *
     * @param modelName model name
     * @param rows the data list to be created
     * @return the data list with `sliceId`
     */
    List<Map<String, Object>> createSlices(String modelName, List<Map<String, Object>> rows);

    /**
     * Delete a slice of timeline model by `sliceId`.
     *
     * @param modelName model name
     * @param timelineSlice the slice object of timeline model
     * @return true / Exception
     */
    boolean deleteSlice(String modelName, TimelineSlice timelineSlice);

    /**
     * Batch update slices by `sliceId` in the list.
     * When updating `effectiveStartDate`, also check and update the `effectiveEndDate` of the affected slices.
     *
     * @param modelName model name
     * @param rows data list to be updated
     * @return the number of rows updated
     */
    Integer updateSlices(String modelName, List<Map<String, Object>> rows);

}
