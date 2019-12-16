package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.dto.SaleMoneyRatioDTO;
import com.bee.platform.cloud.si.manufacture.dto.UnfinishedFinanceDTO;
import com.bee.platform.cloud.si.manufacture.dto.UnfinishedGoodsDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleContractBasic;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 销售合同信息表 Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
public interface SaleContractBasicMapper extends BaseMapper<SaleContractBasic> {

    /**
     * 查询销售总额
     *
     * @param map
     * @return
     */
    List<SaleMoneyRatioDTO> getSaleMoney(Map<String, Object> map);

    /**
     * 查询销售回款总额
     *
     * @param map
     * @return
     */
    List<SaleMoneyRatioDTO> getSaleMoneyBack(Map<String, Object> map);

    /**
     * 查询未完成业务的账款情况-主料、辅料、产品
     * @param map
     * @return
     */
	List<UnfinishedFinanceDTO> getUnfinishedFinance(HashMap<String, Object> map);
	
	/**
	 * 查询未完成业务的账款情况-供应商
	 * @param map
	 * @return
	 */
	List<UnfinishedFinanceDTO> getSupplierUnfinishedFinance(HashMap<String, Object> map);

	/**
	 * 查询未完成业务的发货情况-供应商
	 * @param map
	 * @return
	 */
	List<UnfinishedGoodsDTO> getSupplierUnfinishedGoods(HashMap<String, Object> map);

	/**
	 * 查询未完成业务的发货情况-主料、辅料、成品
	 * @param map
	 * @return
	 */
	List<UnfinishedGoodsDTO> getUnfinishedGoods(HashMap<String, Object> map);

	/**
	 * 查询金额总览
	 *
	 * @param map
	 * @return
	 */
	public BigDecimal getDataScreen(Map<String, Object> map);
}
