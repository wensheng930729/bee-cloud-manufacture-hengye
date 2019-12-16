package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuySample;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

import javax.validation.Valid;

/**
 * <p>
 * 采购取样表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-23
 */
public interface BuySampleService extends IService<BuySample> {

    /**
     * 设置样品检测结果
     *
     * @param userInfo
     * @param sampleSureRQ
     * @return
     */
    public ResponseResult sureBuySampleResult(AuthPlatformUserInfo userInfo, BuySampleSureRQ sampleSureRQ);

    /**
     * 重新化验退回样品
     *
     * @param userInfo
     * @param sampleSureRQ
     * @return
     */
    public ResponseResult sendBackBuySample(AuthPlatformUserInfo userInfo, BuySampleSureRQ sampleSureRQ);

    /**
     * 查询样品信息列表
     *
     * @param
     * @param confirmStatus
     * @param pagination
     * @return
     */
    public ResponseResult<BuySampleInfoDTO> getBuySampleInfoList(AuthPlatformUserInfo userInfo, Integer confirmStatus, Pagination pagination);

    /**
     * 车次样品货物确认（根据磅单id）
     *
     * @param userInfo
     * @param carSampleSureRQ
     * @return
     */
    public ResponseResult sureCarSampleResult(AuthPlatformUserInfo userInfo, BuyCarSampleSureRQ carSampleSureRQ);

    /**
     * 查看车次货物信息列表
     *
     * @param confirmStatus
     * @param pagination
     * @return
     */
    public ResponseResult<BuyCarSampleInfoDTO> getCarSampleInfoList(AuthPlatformUserInfo userInfo, Integer confirmStatus, Pagination pagination);

    /**
     * 采购采购商重新化验
     *
     * @param rq
     * @return
     */
    public ResponseResult<ResCodeEnum> purchaserReAssay(PurchaserReAssayBuyRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 查看样品信息详情
     *
     * @param sampleCode
     * @return
     */
    public ResponseResult<SampleResultDTO> getBuySampleInfo(String sampleCode, AuthPlatformUserInfo userInfo);

    /**
     * 查看同一磅单下样品详细信息
     *
     * @param machineId
     * @return
     */
    public ResponseResult<BuyCarSampleMsgDTO> getCarSampleInfo(String machineId, AuthPlatformUserInfo userInfo);

    /**
     * 合同查看页面，根据合同编号、批次id查看车次样品整体检验情况
     *
     * @param contractBusinessId
     * @param batchId
     * @return
     */
    public List<BuyCarSampleDTO> getCarSimple(String contractBusinessId, String batchId);

    /**
     * 查看不合格车辆信息
     *
     * @param contractBusinessId
     * @param pagination
     * @return
     */
    public ResponseResult<List<BuyUnqualifiedCarDTO>> getUnqualifiedCarList(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 保存取样信息
     *
     * @param rq
     * @return
     */
    public ResponseResult<String> saveSample(SaveSampleBuyRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 完成取样
     */
    public ResponseResult<ResCodeEnum> finishSample(FinishSampleBuyRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 化验人员弃用样品
     */
    public ResponseResult<ResCodeEnum> abandonSample(SampleAssayAbandonRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 化验人员开始化验样品
     */
    public ResponseResult<ResCodeEnum> startAssaySample(SampleAssayStartRQ rq, Object o, AuthPlatformUserInfo userInfo);

    /**
     * 保存样品化验结果
     */
    public ResponseResult<ResCodeEnum> saveAssayResult(SampleAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据样品code获取样品详情
     */
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(Object o, String sampleCode);

    /**
     * web-根据合同号查看车辆信息（已车次货物确认）
     */
	public ResponseResult<List<BuyCarDTO>> getCarList(String contractBusinessId, AuthPlatformUserInfo userInfo,
			Pagination pagination);

}
