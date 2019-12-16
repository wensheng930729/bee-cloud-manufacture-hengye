package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.dto.LookBoarOutStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.RawMaterialDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;

import java.util.List;

/**
 * @ClassName: LookBoardStorageService
 * @Description: 管理后台数据看板库存情况
 * @Author: fei.sun
 * @Date: 2019/10/18 16:09
 * @Version: 1.0
 */
public interface LookBoardStorageService {
    /**
     * 查询原料库存情况
     * @param goodsType 产品类型
     * @param dateTime 查询时间
     * @param userInfo 用户信息
     * @return 产品名称及数量
     */
    List<RawMaterialDTO> selectRawMaterial(Integer goodsType, String dateTime, AuthPlatformUserInfo userInfo);

    /**
     * 看板出库情况
     * @param type 1、按日 2、按月
     * @param goodsType 产品类型
     * @param startTime 查询起始时间
     * @param endTime 查询终止时间
     * @param userInfo 用户信息
     * @return 返回列表
     */
    List<LookBoarOutStorageDTO> selectOutStorage(Integer type, Integer goodsType, String startTime, String endTime
            , AuthPlatformUserInfo userInfo);

    /**
     * 在途量
     * @param goodsType 产品类别
     * @param userInfo 用户信息
     * @return a
     */
    List<RawMaterialDTO> selectInTransit(Integer goodsType, AuthPlatformUserInfo userInfo);
}
