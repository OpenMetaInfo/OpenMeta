package info.openmeta.framework.orm.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.utils.Assert;
import lombok.Getter;

import java.util.Collections;
import java.util.List;

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
     * Whether to scroll infinitely, the default is false
     * Infinite scroll needs to ensure that the data before and after paging is ordered,
     * and 'id ASC' sorting rule will be added to the end of the order by clause.
     */
    @JsonIgnore
    private final boolean scroll;
    // Whether to perform a count query first, return directly when count is 0, default is true
    @JsonIgnore
    private final boolean count;

    // Total number of pages
    private long pages;
    // Total number of rows
    private long total;
    private List<T> rows = Collections.emptyList();

    /**
     * Default first page, page size is 50, and ensure that pageSize is between [1, MAX_BATCH_SIZE]
     * @param pageNumber Current page number
     * @param pageSize Page size
     * @param scroll Whether to scroll infinitely
     * @param count Whether to perform a count query first
     */
    public Page(Integer pageNumber, Integer pageSize, boolean scroll, boolean count) {
        this.pageNumber = pageNumber == null || pageNumber < 1 ? BaseConstant.DEFAULT_PAGE_NUMBER: pageNumber;
        if (pageSize == null || pageSize < 1) {
            this.pageSize = BaseConstant.DEFAULT_PAGE_SIZE;
        } else {
            Assert.isTrue(pageSize <= BaseConstant.MAX_BATCH_SIZE,
                    "Page size {0} cannot exceed the maximum limit: {1}", pageSize, BaseConstant.MAX_BATCH_SIZE);
            this.pageSize = pageSize;
        }
        this.scroll = scroll;
        this.count = count;
    }

    /**
     * Paging object constructor, using default page number and page size, and count total rows.
     * @return Page object
     * @param <T> Generic type
     */
    public static <T> Page<T> of() {
        return new Page<>(BaseConstant.DEFAULT_PAGE_NUMBER, BaseConstant.DEFAULT_PAGE_SIZE, false, true);
    }

    /**
     * Paging object constructor, default count total rows.
     * @param pageNumber Current page number
     * @param pageSize Page size
     * @return Page object
     * @param <T> Generic type
     */
    public static <T> Page<T> of(Integer pageNumber, Integer pageSize) {
        return new Page<>(pageNumber, pageSize, true, true);
    }

    /**
     * Static constructor of paging object.
     * @param pageNumber Current page number
     * @param pageSize Page size
     * @param scroll Whether to scroll infinitely
     * @param count Whether to perform a count query first
     * @return Page object
     * @param <T> Generic type
     */
    public static <T> Page<T> of(Integer pageNumber, Integer pageSize, boolean scroll, boolean count) {
        return new Page<>(pageNumber, pageSize, scroll, count);
    }

    /**
     * When setting the total, calculate the total number of pages at the same time.
     * @param total Total number of rows
     */
    public void setTotal(long total) {
        this.total = total;
        this.pages = (total + pageSize - 1) / pageSize;
    }

    public Page<T> setRows(List<T> rows) {
        this.rows = rows;
        return this;
    }

    /**
     * If there is a next page, go to the next page and return true, otherwise return false.
     * Suitable for do { ... } while (page.toNext()) paging operation, automatically judge and go to the next page;
     * The next page exists condition: 1 < next page number <= total number of pages
     * @return True if there is a next page, otherwise false
     */
    public boolean toNext() {
        pageNumber ++;
        return pageNumber > 1 && pageNumber <= pages;
    }

    /**
     * Switch the generic parameter type in the paging object, generally used for paging queries,
     * @param sourcePage Original paging object
     * @param objects List of parameters of the target type
     * @return New paging object
     */
    public static <T, E> Page<T> rebuild(Page<E> sourcePage, List<T> objects) {
        Page<T> page = Page.of(sourcePage.getPageNumber(), sourcePage.getPageSize());
        page.setRows(objects);
        page.setTotal(sourcePage.getTotal());
        return page;
    }
}
