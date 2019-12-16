import request from '@/utils/request'
import { stringify } from 'querystring';
import { api_user_prefix, api_factory_prefix } from '@/constants/prefix';

/**
 * 采购合同详情
 * @param {*} contractBusinessId  合同业务id
 */
export async function getBuyContractDetail(contractBusinessId) {
  return request(`${api_factory_prefix}/buyContractBasic/getBuyContractDetail?contractBusinessId=${contractBusinessId}`, {
    method: "GET"
  });
}

/**
 * 合同查看-收货情况-查看车辆信息(收货情况)
 * @param {*} contractBusinessId  合同业务id
 * 
 */
export async function getCarList({ contractBusinessId, pageSize, currentPage }) {
  return request(`${api_factory_prefix}/buySample/getCarList?contractBusinessId=${contractBusinessId}&pageSize=${pageSize}&currentPage=${currentPage}`, {
    method: "GET"
  });
}

/**
 * 采购-获取车次样品详细信息（根据磅单id）
 * @param {*} machineId  榜单Id
 * 
 */
export async function getCarSampleInfo({ machineId }) {
  return request(`${api_factory_prefix}/buySample/getCarSampleInfo?machineId=${machineId}`, {
    method: "GET"
  });
}

/**
 * 重新化验
 * @param {*} machineId  磅单id
 * 
 */
export async function purchaserReAssay(params) {
  return request(`${api_factory_prefix}/buySample/purchaserReAssay`, {
    method: "POST",
    body: params
  });
}

/**
 * 完成合同
 * @param {*} businessId  合同id对应数据中contractBusinessId
 */
export async function completeBuyContract(params) {
  return request(`${api_factory_prefix}/buyContractBasic/completeBuyContract`, {
    method: "POST",
    body: params
  });
}

/**
 * 采购合同付款
 * @param {*}
 * contractBusinessId 合同业务id
 * payAmount 付款金额
 * payTime 付款日期
 */
export async function payForBuyContractBasic(params) {
  return request(`${api_factory_prefix}/buyContractBasic/payForBuyContractBasic`, {
    method: "POST",
    body: params
  });
}

/**
 * 合同确认入库
 * @param {*}
 * [
  {
    "machineId": "string"
  }
]
 * machineId 磅单号
 */
export async function bulkConfirmProduct(params) {
  return request(`${api_factory_prefix}/storage/bulkConfirmProduct`, {
    method: "POST",
    body: params
  });
}


/**
 * 折价入库
 * @param {*}
 * discountedPrice 折价后单价
 * machineIds	[待入库车辆榜单id]
 * 
 */
export async function bulkDiscountedProduct(params) {
  return request(`${api_factory_prefix}/storage/bulkDiscountedProduct`, {
    method: "POST",
    body: params
  });
}

/**
 * 采购供销结算完成结算
 */
export async function sureContractSettle(params) {
  return request(`${api_factory_prefix}/buyContractSettlement/sureContractSettle`, {
    method: "POST",
    body: params
  });
}

/**
 * 采购合同修改结算单价和金额
 */
export async function updateSettleAmountBuyContract(params) {
  return request(`${api_factory_prefix}/buyContractBasic/updateSettleAmountBuyContract`, {
    method: "POST",
    body: params
  });
}

/**
 * 获取采购供销结算详情
 * @param {*} contractBusinessId  合同业务id
 */
export async function getContractSettleInfo(contractBusinessId) {
  return request(`${api_factory_prefix}/buyContractSettlement/getContractSettleInfo?contractBusinessId=${contractBusinessId}`, {
    method: "GET"
  });
}

/**
 * 采购供销结算保存合同情况
 */
export async function saveContractSettleInfo(params) {
  return request(`${api_factory_prefix}/buyContractSettlement/saveContractSettleInfo`, {
    method: "POST",
    body: params
  });
}

/**
 * 查询当前角色结算权限
 */
export async function getAuth() {
  return request(`${api_user_prefix}/auth/role/settlement/auth`, {
    method: "GET",
  });
}

/**采购结算相关**/

/**
 * 获取采购结算列表
 * 采购结算列表 settleStatus：0未结算 1已结算
 */
export async function getSettleListBuy(params) {
  return request(`${api_factory_prefix}/buyContractSettlement/getSettleListBuy?${stringify(params)}`, {
    method: "GET",
  });
}

/**
 * 采购结算弹窗 settleStatus：0未结算 1已结算
 */
export async function getSettleBuyPopupWindow(params) {
  return request(`${api_factory_prefix}/buyContractSettlement/getSettleBuyPopupWindow?${stringify(params)}`, {
    method: "GET",
  });
}

/**
 * 采购结算弹窗结算
 */
export async function saveSettleBuyPopupWindow(params) {
  return request(`${api_factory_prefix}/buyContractSettlement/saveSettleBuyPopupWindow`, {
    method: "POST",
    body: params
  });
}