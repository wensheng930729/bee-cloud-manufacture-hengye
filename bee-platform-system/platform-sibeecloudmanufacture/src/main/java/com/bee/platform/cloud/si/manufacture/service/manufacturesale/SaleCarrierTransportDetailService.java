package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleCarDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleCarrierTransportDetail;
import com.bee.platform.cloud.si.manufacture.rq.SaleDiscountTranspoerDetailRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 承运方运输详情表(销售) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleCarrierTransportDetailService extends IService<SaleCarrierTransportDetail> {

    /**
     * 根据承运方运输段ID查询承运方运输详情车次信息
     * @param carrierTransportId
     * @return
     */
    List<SaleTransportDetailDTO> getTransportDetailByTransportId(String carrierTransportId);

    /**
     * 单独保存车次信息
     * @param transportDetailDTO
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveTransportDetailByTransport(SaleTransportDetailDTO transportDetailDTO, AuthPlatformUserInfo userInfo);

    /**
     * 查询批次下未到货的车辆信息
     * @param batchId
     * @return
     */
    ResponseResult<List<SaleTransportDetailDTO>> getNotArrivalTransportDetail(String batchId);

    /**
     * 保存被折价的车辆单价信息
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveDiscountTransportDetail(SaleDiscountTranspoerDetailRq rq, AuthPlatformUserInfo userInfo);

    /**
     * web 查看销售合同下收货情况
     * @return
     */
	ResponseResult<List<SaleCarDTO>> getCarList(String contractBusinessId, AuthPlatformUserInfo userInfo,
			Pagination pagination);


}
