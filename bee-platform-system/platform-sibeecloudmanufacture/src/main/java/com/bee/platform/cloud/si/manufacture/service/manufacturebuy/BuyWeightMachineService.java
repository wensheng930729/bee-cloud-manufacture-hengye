package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuyWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import sun.security.util.AuthResources_ja;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;


/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息 服务类
 * @Date 2019/9/23 13:48
 */
public interface BuyWeightMachineService extends IService<BuyWeightMachine> {

    /**
     * @Description 保存地磅数据信息
     * @author chenxm66777123
     * @Date 2019/9/24 9:23
     * @version 1.0.0
     */
    ResponseResult<String> saveBuyWeightMachine(WeightMachineRq weightMachineRq, AuthPlatformUserInfo userInfo, Integer dataSource);

    /**
     * @Description 根据企业查询称重信息
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    List<WeightMachineDTO> getBuyWeightMachineList(Integer orgId, Pagination pagination);

    /**
     * 查询过磅单数据信息
     * @param rq
     * @param userInfo
     * @param pagination
     * @return
     */
    List<WeighingListDTO> getWeighListInfo(WeighingListRq rq, AuthPlatformUserInfo userInfo, Pagination pagination);

    /**
     * @Description 根据企业获取已称重详情（总览）
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    List<WeightMachineTotalDTO> getWeightMachineListTotal(Integer orgId, Pagination pagination);


    /**
     * @Description 根据企业获取已称重详情（详细信息）
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    List<WeightMachineDTO> getWeightMachineListDetails(Integer orgId, String contractNum, Pagination pagination);

    /**
     * @Description 开始称重
     * @author chenxm66777123
     * @Date 2019/9/24 15:42
     * @version 1.0.0
     */
    String getWeight(String deviceId);

    /**
     * 多车一样界面根据产品id查询未完成合同的磅单号下拉列表
     *
     * @param productId
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyWeightMachineBoxDTO>> getWeitghtIdByProductId(Integer productId, AuthPlatformUserInfo userInfo);

    /**
     * 根据地磅id修改车次的折扣单价
     *
     * @param machineId
     * @param unitPrice
     * @param userInfo
     */
    void updateDiscountUnitPrice(String machineId, BigDecimal unitPrice, AuthPlatformUserInfo userInfo);


    /**
     * @Description 保存称重信息
     * @author chenxm66777123
     * @Date 2019/9/28 14:15
     * @version 1.0.0
     */
    ResponseResult<String> buyConfirmWeight(ConfirmWeightMachineRq confirmWeightMachineRq, AuthPlatformUserInfo userInfo);


    /**
     * @Description 通知取样
     * @author chenxm66777123
     * @Date 2019/9/30 10:15
     * @version 1.0.0
     */
    ResponseResult<String> samplePush(String machineId, AuthPlatformUserInfo userInfo);

    /**
     * @Description 结束磅单
     * @author chenxm66777123
     * @Date 2019/10/8 9:25
     * @version 1.0.0
     */
    ResponseResult<String> confirmMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo);

    /**
     * @Description 继续运载
     * @author chenxm66777123
     * @Date 2019/10/8 9:25
     * @version 1.0.0
     */
    ResponseResult<String> continueMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo);

    /**
     * @Description 删除磅单
     * @author chenxm66777123
     * @Date 2019/10/8 9:25
     * @version 1.0.0
     */
    ResponseResult<String> delMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo);

    /**
     * @Description 六个小时执行一次删除数据任务
     * @author chenxm66777123
     * @Date 2019/10/21 9:22
     * @version 1.0.0
     */
    void clearWeightData();

    /**
     * 多车一样界面 查询从未取样的磅单车牌号下拉列表
     *
     * @param userInfo
     * @return
     */
    ResponseResult<List<SampleMachineBuyDTO>> getSampleMachineBox(Integer productId,AuthPlatformUserInfo userInfo);

    /**
     * 根据车牌号查询合同磅单信息
     * @param trainNumber
     * @param userInfo
     * @return
     */
    public ResponseResult<SampleMachineBuyDTO> getMachineByTrainNum(String trainNumber,AuthPlatformUserInfo userInfo);
    /**
     * @Description 确认扣重
     * @author chenxm66777123
     * @Date 2019/11/5 9:27
     * @version 1.0.0
     */
    ResponseResult<String> confirmDeductWeight(String machineId, BigDecimal deductWeight, Integer deductWeightByManual, AuthPlatformUserInfo userInfo);

    /**
     * @Description 根据车牌号，自动带出最近一次该车所对应的承运商。
     * @author chenxm66777123
     * @Date 2019/11/11 9:51
     * @version 1.0.0
     */
    ResponseResult<CarrierInfoDTO> getLastCarrierByTrainNumber(String trainNumber, AuthPlatformUserInfo userInfo);

    /**
    * @Description 保存备注
    * @author chenxm66777123
    * @Date 2019/11/15 9:56
    * @version 1.0.0
    */
    ResponseResult<String> saveRemark(String machineId, String remark,AuthPlatformUserInfo userInfo);



    /**
     * @Description 修改车牌号
     * @author chenxm66777123
     * @Date 2019/12/3 11:09
     * @version 1.0.0
     */
    ResponseResult<String> updateTrainNumber(String machineId, String trainNumber,AuthPlatformUserInfo userInfo);



    /**
     * @Description web端地磅头部车辆、净重统计
     * @author chenxm66777123
     * @Date 2019/11/25 10:42
     * @version 1.0.0
     */
    ResponseResult<WeightMachineWebCountDTO> getWeightMachineWebCount(Integer isWeight,AuthPlatformUserInfo userInfo);


    /**
     * @Description web获取待过磅/已过磅列表信息
     * @author chenxm66777123
     * @Date 2019/11/25 14:12
     * @version 1.0.0
     */
    List<WeightMachineWebDTO> getWeightMachineWebList(WeightMachineWebListRq rq,AuthPlatformUserInfo userInfo,Pagination pagination);

    /**
     * @Description web根据磅单号获取详细信息
     * @author chenxm66777123
     * @Date 2019/11/25 16:04
     * @version 1.0.0
     */
    ResponseResult<WeightMachineWebDTO> getWeightMachineWebDeatil(String machineId);

    /**
     * @Description 磅单绑定合同号列表查询
     * @author chenxm66777123
     * @Date 2019/11/26 10:28
     * @version 1.0.0
     */
    List<WeightMachineWebBindDTO> getWeightMachineWebBindList(WeightMachineBindRq rq,AuthPlatformUserInfo userInfo,Pagination pagination);

    /**
     * @Description web磅单获取管理的合同号
     * @author chenxm66777123
     * @Date 2019/11/26 15:14
     * @version 1.0.0
     */
    List<WeightMachineWebRelationContractDTO> getRelationContract(AuthPlatformUserInfo userInfo,String custOrSupName,String productName,Pagination pagination);

    /**
     * @Description web磅单忽略/恢复
     * @author chenxm66777123
     * @Date 2019/11/26 15:55
     * @version 1.0.0
     */
    ResponseResult<String> ignoreRecoveryMachine(WeightIgnoreMachineRq rq);

    /**
     * @Description web磅单绑定合同号
     * @author chenxm66777123
     * @Date 2019/11/26 17:10
     * @version 1.0.0
     */
    void batchBindContract(List<Map<String,Object>> param);
}
