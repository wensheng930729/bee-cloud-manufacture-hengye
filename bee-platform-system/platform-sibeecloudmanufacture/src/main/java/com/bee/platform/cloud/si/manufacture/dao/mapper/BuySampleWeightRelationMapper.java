package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.dto.DetailContractStorageBuyDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuySampleWeightRelation;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 采购样品榜单关联关系 Mapper 接口
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-25
 */
public interface BuySampleWeightRelationMapper extends BaseMapper<BuySampleWeightRelation> {

    /**
     * 二维码扫描获取详情-根据样品code获取合同仓库详情相关数据
     *
     * @param sampleCode 样品code
     * @return
     */
    List<DetailContractStorageBuyDTO> getContractStorageDetailList(@Param("sampleCode") String sampleCode);

}
