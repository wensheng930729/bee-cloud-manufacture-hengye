package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyWeightMachineBoxDTO;
import com.bee.platform.cloud.si.manufacture.dto.WeighingListDTO;
import com.bee.platform.cloud.si.manufacture.dto.WeightMachineTotalDTO;
import com.bee.platform.cloud.si.manufacture.dto.WeightMachineWebBindDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyWeightMachine;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;


/**
 * @Description 磅单附件信息 Mapper 接口
 * @author chenxm66777123
 * @Date 2019/9/23 13:50
 * @version 1.0.0
 */
public interface BuyWeightMachineMapper extends BaseMapper<BuyWeightMachine> {

    List<BuyWeightMachineBoxDTO> getWeitghtIdByProductId(Map map);

    List<WeightMachineTotalDTO> getWeightMachineListTotal(Integer orgId, Pagination pagination);

    /**
     * 根据批次id查询批次到货数量
     * @param batchId
     * @return
     */
    BigDecimal getArrivalVolumeByBatchId(@Param("batchId") String batchId);

    /**
     * 根据合同id查询批次到货数量
     * @param contractBusinessId
     * @return
     */
    BigDecimal getArrivalVolumeByContractId(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 查询过磅单数据信息
     * @param params
     * @return
     */
    List<WeighingListDTO> getWeighListInfo(Map<String, Object> params, Pagination pagination);

    /**
     * @Description 磅单绑定合同号列表查询
     * @author chenxm66777123
     * @Date 2019/11/26 11:01
     * @version 1.0.0
     */
    List<WeightMachineWebBindDTO> getWeightMachineWebBindList(Map<String, Object> params, Pagination pagination);

    /**
     * @Description web磅单绑定合同号
     * @author chenxm66777123
     * @Date 2019/11/26 17:14
     * @version 1.0.0
     */
     void batchBindContract(List<Map<String, Object>> param);
}
