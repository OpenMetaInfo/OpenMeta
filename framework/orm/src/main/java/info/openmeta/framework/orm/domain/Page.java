package info.openmeta.framework.orm.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.entity.BaseModel;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Paging object
 * @param <T>
 */
@Getter
public class Page<T> {

    // Current page number, the first page starts counting from 1
    private int pageNumber;

    // Page size, the default is 50, and the page size is between [1, MAX_BATCH_SIZE]
    private final int pageSize;

    /**
     * Enables cursor-based pagination for improved stability and performance during data pagination.
     * In cursor pagination, the system tracks the last processed record's unique identifier, such as `lastMaxId`,
     * to fetch the next set of records. This approach ensures stable pagination by avoiding the limitations
     * of offset-based pagination, such as skipping records or performance degradation in large datasets.
     * <p>
     * When using cursor pagination, the backend retrieves the next page of data using the condition
     * `WHERE id > lastMaxId`, ensuring that the query remains efficient, especially in cases of large datasets
     * where offset-based pagination would otherwise require scanning and skipping many rows.
     * <p>
     * By default, backend data processing uses cursor pagination for efficiency.
     * However, client-side APIs continue to use traditional offset pagination.
     * Cursor pagination is only activated when the paging parameters include `lastMaxId`, which signals the backend
     * to switch from offset-based pagination to cursor-based pagination for improved query performance.
     * <p>
     * When additional sorting conditions are specified, such as `ORDER BY name ASC, code DESC`, and the `id`
     * is sequentially increasing, cursor-based pagination can still be applied.
     * In this case, the SQL query needs to account for both the sorting fields (`name`, `code`) and the unique `id`
     * to ensure proper ordering and pagination stability.
     * <p>
     * For example, consider the following SQL query:
     * - Offset Pagination:
     *   `SELECT * FROM articles WHERE (other filters) ORDER BY name ASC, code DESC LIMIT 10 OFFSET 50`
     * <p>
     * - Cursor Pagination (when `lastMaxId`, `name`, and `code` are provided):
     *   `SELECT * FROM articles
     *    WHERE (other filters)
     *          AND (name > 'John'
     *              OR (name = 'John' AND code < 'X001')
     *              OR (name = 'John' AND code = 'X001' AND id > lastMaxId))
     *    ORDER BY name ASC, code DESC LIMIT 10`
     * <p>
     * In this query:
     * - The system continues sorting by `name ASC` and `code DESC`.
     * - The `name > 'John'` condition fetches records where the name is alphabetically after "John".
     * - The `name = 'John' AND code < 'X001'` condition handles records where the name is the same but the `code` is in descending order.
     * - The `id > lastMaxId` ensures that, when both `name` and `code` are the same, records after the last processed `id` are retrieved, maintaining the correct order and avoiding duplicates.
     * <p>
     * This structure ensures stable and efficient cursor-based pagination, even with complex sorting and filtering criteria.
     */
    @JsonIgnore
    private final boolean cursorPage;

    // Whether to perform a count query first, return directly when count is 0, default is true
    @JsonIgnore
    @Setter
    private boolean count;

    // Total number of pages, calculated by (total + pageSize - 1) / pageSize
    private long pages;

    // Total number of rows
    private long total;

    private List<T> rows = Collections.emptyList();

    @JsonIgnore
    private int currentPageSize;

    @JsonIgnore
    private Serializable lastMaxId;

    /**
     * Default first page, page size is 50, and ensure that pageSize is between [1, MAX_BATCH_SIZE]
     *
     * @param pageNumber Current page number
     * @param pageSize Page size
     * @param cursorPage Whether to enforce a stable order
     * @param count Whether to perform a count query first
     */
    public Page(Integer pageNumber, Integer pageSize, boolean cursorPage, boolean count) {
        if (cursorPage) {
            // In cursor pagination, pageNumber is irrelevant, so it can be ignored.
            this.pageNumber = BaseConstant.DEFAULT_PAGE_NUMBER;
        } else {
            this.pageNumber = pageNumber == null || pageNumber < 1 ? BaseConstant.DEFAULT_PAGE_NUMBER : pageNumber;
        }
        if (pageSize == null || pageSize < 1) {
            this.pageSize = BaseConstant.DEFAULT_PAGE_SIZE;
        } else {
            Assert.isTrue(pageSize <= BaseConstant.MAX_BATCH_SIZE,
                    "Page size {0} cannot exceed the maximum limit: {1}", pageSize, BaseConstant.MAX_BATCH_SIZE);
            this.pageSize = pageSize;
        }
        this.cursorPage = cursorPage;
        this.count = count;
    }

    /**
     * Stable cursor-based paging object constructor, the default page size is 50, without count query.
     *
     * @return Stable cursor-based paging object
     * @param <T> Generic type
     */
    public static <T> Page<T> ofCursorPage() {
        return new Page<>(null, BaseConstant.DEFAULT_PAGE_SIZE, true, false);
    }

    /**
     * Stable cursor-based paging object constructor with custom page size, without count query.
     *
     * @return Stable cursor-based paging object
     * @param <T> Generic type
     */
    public static <T> Page<T> ofCursorPage(Integer pageSize) {
        return new Page<>(null, pageSize, true, false);
    }

    /**
     * Offset-based paging object constructor, default count total rows.
     * Typically used for client-side API paging queries.
     *
     * @param pageNumber Current page number
     * @param pageSize Page size
     * @return Page object
     * @param <T> Generic type
     */
    public static <T> Page<T> of(Integer pageNumber, Integer pageSize) {
        return new Page<>(pageNumber, pageSize, false, true);
    }

    /**
     * Offset-based paging object constructor with specified count parameter.
     * Compatible with traditional offset-based paging queries.
     *
     * @param pageNumber Current page number
     * @param pageSize Page size
     * @param count Whether to perform a count query first
     * @return Page object
     * @param <T> Generic type
     */
    public static <T> Page<T> of(Integer pageNumber, Integer pageSize, boolean count) {
        return new Page<>(pageNumber, pageSize, false, count);
    }

    /**
     * When setting the total, calculate the total number of pages at the same time.
     *
     * @param total Total number of rows
     */
    public void setTotal(long total) {
        this.total = total;
        this.pages = (total + pageSize - 1) / pageSize;
    }

    /**
     * Set the rows of the current page and calculate the current page size.
     *
     * @param rows List of rows
     * @return Page object
     */
    public Page<T> setRows(List<T> rows) {
        this.rows = rows;
        if (rows != null) {
            this.currentPageSize = rows.size();
        }
        return this;
    }

    /**
     * If there is a next page, go to the next page and return true, otherwise return false.
     * Suitable for do { ... } while (page.toNext()) paging operation, automatically judge and go to the next page.
     * <p>
     *     * Cursor-based paging:
     *      The next page might exist condition: `currentPageSize == pageSize`
     *     * Offset-based paging:
     *      The next page exists condition: `pageNumber > 1 && pageNumber <= pages`.
     *
     * @return True if there is a next page, otherwise false
     */
    public boolean toNext() {
        if (cursorPage) {
            boolean hasNext = currentPageSize == pageSize;
            if (hasNext) {
                T lastObject = rows.getLast();
                if (lastObject instanceof Map) {
                    lastMaxId = (Serializable) ((Map<?, ?>) lastObject).get(ModelConstant.ID);
                } else if (lastObject instanceof BaseModel) {
                    lastMaxId = ((BaseModel) lastObject).getId();
                }
            }
            return currentPageSize == pageSize;
        } else {
            pageNumber ++;
            return pageNumber > 1 && pageNumber <= pages;
        }
    }
}
