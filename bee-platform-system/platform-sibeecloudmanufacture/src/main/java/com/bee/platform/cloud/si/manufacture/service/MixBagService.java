package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.dto.TonBagDetailDTO;
import com.bee.platform.cloud.si.manufacture.rq.BaggingUpdateRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.math.BigDecimal;

/**
 * @ClassName: MixBagService
 * @Description: 混袋Service
 * @Author: fei.sun
 * @Date: 2019/11/25 15:19
 * @Version: 1.0
 */
public interface MixBagService {

    /**
     *  确认混袋
     * @param majorTonBagNum 主吨袋编号
     * @param tonBagNum 混袋吨袋编号
     * @param mixBagAmount 混袋数量
     * @param userInfo 用户信息
     */
    void confirmMixBag(String majorTonBagNum, String tonBagNum, BigDecimal mixBagAmount, AuthPlatformUserInfo userInfo);

    /**
     * 根据吨袋编号查询吨袋相关信息
     * @param tonBagNumber 吨袋编号
     * @return
     */
    ResponseResult<TonBagDetailDTO> getTonBagDetail(String tonBagNumber);

    /**
     * 更新吨袋相关信息
     * @param rq 吨袋信息修改入参
     * @param userInfo 用户信息
     * @return
     */
    ResponseResult<String> updateTonBagInfo(BaggingUpdateRq rq, AuthPlatformUserInfo userInfo);

}
