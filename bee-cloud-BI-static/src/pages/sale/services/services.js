import request from '@/utils/request';
import { api_user_prefix, api_factory_prefix } from '@/constants/prefix';

/**
 * 销售合同详情
 * @param {*} contractBusinessId  合同业务id
 */
export async function getSaleContractDetail(contractBusinessId) {
  return request(
    `${api_factory_prefix}/saleContractBasic/getSaleContractDetail?contractBusinessId=${contractBusinessId}`,
    {
      method: 'GET',
    },
  );
}

/**
 * 合同查看-收货情况-查看车辆信息(收货情况)
 * @param {*} contractBusinessId  合同业务id
 *
 */
export async function getCarList({ contractBusinessId, pageSize, currentPage }) {
  return request(
    `${api_factory_prefix}/saleCarrierTransportDetail/getCarList?contractBusinessId=${contractBusinessId}&pageSize=${pageSize}&currentPage=${currentPage}`,
    {
      method: 'GET',
    },
  );
}

/**
 * 销售-获取车次样品详细信息（根据磅单id）
 * @param {*} machineId  榜单Id
 *
 */
export async function getCarSampleInfo({ carrierTransportDetailId }) {
  return request(
    `${api_factory_prefix}/saleCarrierTransportDetail/getAssayResult?carrierTransportDetailId=${carrierTransportDetailId}`,
    {
      method: 'GET',
    },
  );
}

/**
 * 完成合同
 * @param {*} businessId  合同id对应数据中contractBusinessId
 */
export async function completeSaleContract(params) {
  return request(`${api_factory_prefix}/saleContractBasic/completeSaleContract`, {
    method: "POST",
    body: params
  });
}

/**
 * 销售合同付款
 * @param {*}
 * contractBusinessId 合同业务id
 * payAmount 付款金额
 * payTime 付款日期
 */
export async function receiveForSaleContract(params) {
  return request(`${api_factory_prefix}/saleContractBasic/receiveForSaleContract`, {
    method: 'POST',
    body: params,
  });
}

/**
 * 合同确认入库
 * @param {*}
 {
  "assayResult": 0,
  "carrierTransportDetailIds": [
    "string"
  ],
  "resultList": [
    {
      "assayItem": "string",
      "assayValue": 0,
      "markIn": "string",
      "testUnit": 0,
      "type": 0,
      "unitString": "string"
    }
  ]
}
 * machineId 磅单号
 */
export async function saveAssayResult(params) {
  return request(`${api_factory_prefix}/saleCarrierTransportDetail/saveAssayResult`, {
    method: 'POST',
    body: params,
  });
}

/**
 * 折价入库
 * @param {*}
 *  "carrierTransportDetailIds": [
    "string"
  ],
  "discountUnitPrice": 0
 * discountUnitPrice 折价后单价
 *
 *
 */
export async function saveDiscountTransportDetail(params) {
  return request(
    `${api_factory_prefix}/saleCarrierTransportDetail/saveDiscountTransportDetail`,
    {
      method: 'POST',
      body: params,
    },
  );
}

/**
 * 获取采购供销结算详情
 * @param {*} contractBusinessId  合同业务id
 */
export async function getContractSettleInfo(contractBusinessId) {
  return request(
    `${api_factory_prefix}/buyContractSettlement/getContractSettleInfo?contractBusinessId=${contractBusinessId}`,
    {
      method: 'GET',
    },
  );
}

/**
 * 销售供销结算保存合同情况
 */
export async function saveContractSettleInfo(params) {
  return request(`${api_factory_prefix}/saleContractSettlement/saveContractSettleInfo`, {
    method: 'POST',
    body: params,
  });
}

/**
 * 查询当前角色结算权限
 */
export async function getAuth() {
  return request(`${api_user_prefix}/auth/role/settlement/auth`, {
    method: 'GET',
  });
}
