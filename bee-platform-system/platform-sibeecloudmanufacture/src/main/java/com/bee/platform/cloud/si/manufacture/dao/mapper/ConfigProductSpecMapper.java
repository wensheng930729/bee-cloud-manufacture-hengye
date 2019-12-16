package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.dto.ProductSpecParam;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProductSpec;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 产品规格表 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigProductSpecMapper extends BaseMapper<ConfigProductSpec> {

    /**
     * @notes: 当前用户查询产品规格
     * @Author: junyang.li
     * @Date: 13:59 2019/11/26
     * @param param : 查询参数可以为空
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigProductSpec>
     */
    List<ConfigProductSpec> getProductSpecByProductIds(@Param("param")ProductSpecParam param);
}
