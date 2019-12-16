package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 采购取样表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-23
 */
public interface SampleService {
    /**
     * 查询待取样品列表
     */
    public ResponseResult<List<SamplePrepareDTO>> getSamplePrepareList(AuthPlatformUserInfo userInfo, Page page);

    /**
     * 查询已取样列表
     */
    public ResponseResult<List<SampleAlreadyDTO>> getSampleAlreadyList(AuthPlatformUserInfo userInfo, Page page);

    /**
     * 查询各种状态化验列表
     */
    public ResponseResult getAssayList(AuthPlatformUserInfo userInfo, Integer assayStatus, Page page);

    /**
     * 化验弃样
     */
    public ResponseResult<ResCodeEnum> abandonSample(AuthPlatformUserInfo userInfo, SampleAssayAbandonRQ rq);

    /**
     * 化验开始化验
     */
    public ResponseResult<ResCodeEnum> startAssaySample(AuthPlatformUserInfo userInfo, SampleAssayStartRQ rq);

    /**
     * 化验保存化验结果
     */
    public ResponseResult<ResCodeEnum> saveAssayResult(AuthPlatformUserInfo userInfo, SampleAssayResultSaveRQ rq);
    /**
     * 化验保存临时化验结果
     */
    public ResponseResult<ResCodeEnum> saveTemporaryAssayResult(AuthPlatformUserInfo userInfo, SampleAssayResultSaveRQ rq);

    /**
     * 根据样品code获取临时化验结果
     */
    public ResponseResult<List<SampleAssayResultDTO>> getTemporaryAssayResult(String sampleCode);
    /**
     * 根据样品code获取样品详情
     */
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(AuthPlatformUserInfo userInfo, @RequestParam(required = true) String sampleCode);

    /**
     * 生成样品code excel
     */
    public ResponseResult<List<String>> generateSampleCode(SampleCodeRQ rq);

    /**
     * 查询样品输出项,并计算保存输出项结果
     */
    public void computeAndsaveAssayResult(String sampleCode, Integer productId, List<SampleAssayResultDTO> resultList, Integer businessType, AuthPlatformUserInfo userInfo);

    /**
     * 计算化验输出项结果
     */
    public Double computeAssayResultOutValue(Map<String, Double> inMap, String assayFormula);

    /**
     * 根据samplecode 查询业务线
     */
    public Object getSampleEntityBySampleCode(String sampleCode);

    /**
     * 根据samplecode 查询改code是否使用
     *
     * @return true使用 false未使用
     */
    public boolean checkSampleCodeUsed(String sampleCode);

    /**
     * 根据样品code查询样品化验输出结果
     */
    public ResponseResult<List<SampleAssayResultOutDTO>> getSampleAssayResultOut(AuthPlatformUserInfo userInfo, String sampleCode);

    /**
     * 保存样品化验规格
     */
    public ResponseResult<ResCodeEnum> saveSampleProductSpec(AuthPlatformUserInfo userInfo, SampleSaveProductSpecRQ rq);

    /**
     * 根据编码查询详情
     *
     * @param sampleCode
     * @return
     */
    public ResponseResult getDetailByCode(String sampleCode);
}
