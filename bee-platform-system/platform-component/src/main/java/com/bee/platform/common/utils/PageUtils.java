package com.bee.platform.common.utils;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.Page;
import org.springframework.util.StringUtils;

import java.util.ArrayList;

/**
 * @description: page工具类
 * @author: junyang.li
 * @create: 2018-11-19 16:30
 **/
public class PageUtils {

    public static final Integer DEFAULT_PAGESIZE = 5;

    private static final String ORDER_STAGE_SPLITTER = ",";

    private static final String POINT = "\\.";

    private static final String ASC = "asc";

    private static final String DESC = "desc";

    /**
     * 将公共page对象转换成 mybatis-plus 的Pagination分页对象
     *
     * @param page  公共page 对象
     * @param defaultPageSize   默认分页数
     * @return
     */
    public static Pagination transFromPage(Page page, Integer defaultPageSize) {
        Pagination result = new Pagination();
        if (null == page) {
            return result;
        }
        result.setSize(null == page.getPageSize() || page.getPageSize() <= 0 ? defaultPageSize : page.getPageSize());
        result.setCurrent(null == page.getCurrentPage() || page.getCurrentPage() <= 0 ? 1 : page.getCurrentPage());
        result.setSearchCount(null == page.getSearchCount() || page.getSearchCount());
        result.setAscs(new ArrayList<>());
        result.setDescs(new ArrayList<>());
        if (!StringUtils.isEmpty(page.getOrderStage())) {
            for (String s : page.getOrderStage().split(ORDER_STAGE_SPLITTER)) {
                String[] args = s.split(POINT);
                switch (args[1].toLowerCase()) {
                    case ASC:
                        result.getAscs().add(args[0]);
                        break;
                    case DESC:
                        result.getDescs().add(args[0]);break;

                    default:result.getDescs().add(args[0]);
                }
            }
        }
        return result;
    }

    /**
     * mybatis-plus 分页对象转换成 page 对象
     * @param page 插件分页对象
     * @return page 对象
     */
    public static Page transToPage(Pagination page) {
        Page result = new Page();
        if (null == page) {
            return result;
        }
        result.setCurrentPage(page.getCurrent());
        result.setPageSize(page.getSize());
        result.setTotalPage(page.getPages());
        result.setTotalRecords(page.getTotal());
        return result;
    }

    /**
     * 将 page 转换成 分页插件对象
     * @param page  page 对象
     * @return 插件分页对象
     */
    public static Pagination transFromPage(Page page) {
        return PageUtils.transFromPage(page, DEFAULT_PAGESIZE);
    }
}
