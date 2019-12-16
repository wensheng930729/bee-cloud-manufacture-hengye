package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ProIngredientDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProIngredientStatisticDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredient;
import com.bee.platform.cloud.si.manufacture.rq.ProIngredientRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 配料主表 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
public interface ProIngredientService extends IService<ProIngredient> {
    ResponseResult addIngredient(ProIngredientRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<List<ProIngredientDTO>> findList(ProIngredientRQ rq, AuthPlatformUserInfo userInfo, Pagination pagination);

    ResponseResult<List<ProIngredientStatisticDTO>> findBlankingList(AuthPlatformUserInfo userInfo, Integer havePlc);
    /**
     * @notes: 根据plc查询最后一次保存料批的时间
     * @Author: junyang.li
     * @Date: 15:19 2019/10/12
     * @param plcId :
     * @return: java.util.Date
     */
    Date getLastPreserve(int plcId);


}
