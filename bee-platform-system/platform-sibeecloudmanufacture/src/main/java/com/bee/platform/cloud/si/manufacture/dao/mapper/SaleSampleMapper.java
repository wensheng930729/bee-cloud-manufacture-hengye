package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.SampleAlreadyDTO;
import com.bee.platform.cloud.si.manufacture.dto.SamplePrepareDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleSaleAddContractBusinessIdDTO;
import com.bee.platform.cloud.si.manufacture.dto.TonContractRelationDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleSample;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 销售取样表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-09-26
 */
public interface SaleSampleMapper extends BaseMapper<SaleSample> {

    /**
     * 查询全部业务线待取样列表
     *
     * @param pagination
     * @param map
     * @return
     */
    public List<SamplePrepareDTO> getSamplePrepareList(Pagination pagination, Map map);

    /**
     * 查询全部业务线已取样列表
     *
     * @param pagination
     * @param map
     * @return
     */
    public List<SampleAlreadyDTO> getSampleAlreadyList(Pagination pagination, Map map);

    /**
     * 通过吨袋编号查询 吨袋和合同关联关系
     *
     * @param tonCodeList
     * @return
     */
    public List<TonContractRelationDTO> getTonCodeContractBusinessIdRelation(List<String> tonCodeList);

    /**
     * 通过吨袋编号查询 与吨袋关联的销售样品
     *
     * @param tonCodeList
     * @return
     */
    @Deprecated
    public List<SampleSaleAddContractBusinessIdDTO> getSampleSaleByTonCode(List<String> tonCodeList);
}
