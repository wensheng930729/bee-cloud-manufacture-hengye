package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductCategoryDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProductCategory;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySearchRQ;

import java.util.List;

/**
 * <p>
 * 产品类别 Mapper 接口
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigProductCategoryMapper extends BaseMapper<ConfigProductCategory> {

    /**
     * 条件查询产品分类列表
     * @param pagination 分页对象
     * @param rq 请求参数
     * @return 产品分类列表
     */
    List<ConfigProductCategoryDTO> selectListByCondition(Pagination pagination, ConfigProductCategorySearchRQ rq);
}
