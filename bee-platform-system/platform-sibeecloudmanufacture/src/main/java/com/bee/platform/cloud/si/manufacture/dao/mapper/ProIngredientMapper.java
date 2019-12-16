package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.ProIngredient;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.Date;

/**
 * <p>
 * 配料主表 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
public interface ProIngredientMapper extends BaseMapper<ProIngredient> {
    /**
     * @notes: 根据plc查询最后一次保存料批的时间
     * @Author: junyang.li
     * @Date: 15:19 2019/10/12
     * @param plcId :
     * @return: java.util.Date
     */
    Date getLastPreserve(int plcId);
}
