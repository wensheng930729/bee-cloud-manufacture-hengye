package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.WeightMachineTotalDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleWeightMachine;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @Description 地磅数据信息 Mapper 接口
 * @author chenxm66777123
 * @Date 2019/9/26 16:06
 * @version 1.0.0
 */
public interface SaleWeightMachineMapper extends BaseMapper<SaleWeightMachine> {

    List<WeightMachineTotalDTO> getWeightMachineListTotal(Integer orgId, Pagination pagination);

    /**
     * 通过合同id查询称重总重量
     * @param contractBusinessId
     * @return
     */
    BigDecimal getTotalByContract(@Param("contractBusinessId") String contractBusinessId);

    /**
     * @Description web磅单绑定合同号
     * @author chenxm66777123
     * @Date 2019/11/26 17:14
     * @version 1.0.0
     */
    void batchBindContract(List<Map<String, Object>> param);
}
