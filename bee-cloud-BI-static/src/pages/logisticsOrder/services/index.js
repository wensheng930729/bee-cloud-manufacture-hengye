import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_user_prefix, api_factory_prefix } from '@/constants/prefix';

const prefix = `${api_factory_prefix}`;

// 获取 起点 终点
export async function getLocationList(params) {
  return request(`${prefix}/configLocation/getLocationList?${stringify(params)}`);
}
/*
获取  物流订单详情 // requestType =saleLogisticsBatch/buyLogisticsBatch  分为采购和销售

*/
export async function getLogisticsBatchSectionInfo(params) {
  const { requestType, ...rest } = params;
  return request(`${prefix}/${requestType}/getLogisticsBatchSectionInfo?${stringify(rest)}`);
}

// ------------------------新增批次及阶段信息
export async function saveLogisticsSectionInfo(params = {}) {
  const { requestType, ...rest } = params;
  return request(`${prefix}/${requestType}/saveLogisticsSectionInfo`, {
    method: 'POST',
    body: rest,
  });
}

/*
 requestType =saleTransportSection/buyTransportSection

获取阶段下面的详情 {transportSectionId }

*/

export async function getTransportSectionAllDetail(params) {
  const { requestType, ...rest } = params;
  return request(`${prefix}/${requestType}/getTransportSectionAllDetail?${stringify(rest)}`);
}
/*
保存承运商信息
*/
export async function saveTransportSection(params = {}) {
  const { requestType, ...rest } = params;
  return request(`${prefix}/${requestType}/saveTransportSection`, {
    method: 'POST',
    body: rest,
  });
}
// 获取承运商下拉列表
export async function getCarrierInfoList(params) {
  return request(`${api_user_prefix}/supplier/getCarrierInfoList?${stringify(params)}`);
}
