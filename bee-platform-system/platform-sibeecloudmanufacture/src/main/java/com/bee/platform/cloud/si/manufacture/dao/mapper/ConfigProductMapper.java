package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.ConfigProduct;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 产品档案 Mapper 接口
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigProductMapper extends BaseMapper<ConfigProduct> {
    /**
     * @notes: 查询当前工厂所有的产品类别，暂时只返回产品id 和产品名称，如有需要可自行添加
     * @Author: junyang.li
     * @Date: 10:28 2019/11/26
     * @param factoryId : 当前操作人工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigProduct>
     */
    List<ConfigProduct> getProductList(Integer factoryId);
}
