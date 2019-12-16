import { Component } from 'react';
import { Form, Card, Button, Divider } from 'antd';
import styles from './index.less';
import withRouter from 'umi/withRouter';
import router from 'umi/router';
import * as utils from '@/utils/utils';
import {
  getWeightMachineWebDeatil,
} from '../services/index';

const FormItem = Form.Item;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    detail: {},
  }

  componentDidMount() {
    const { machineId } = this.props.location.query;
    const str = utils.queryString({ machineId, type: 1 });
    getWeightMachineWebDeatil(str).then(res => {
      if (res && res.code === 1) {
        this.setState({
          detail: res.object
        })
      }
    })
  }

  render() {
    const { detail } = this.state;
    return (
      <div className={styles.container}>
        <Card title="过磅车辆" bordered={false}>
          <div className={styles.formPart}>
            <span className={styles.partTitle}>车辆信息</span>
            <Form layout="inline">
              <FormItem label="车牌号">
                {detail.trainNumber || '未知'}
              </FormItem>
              <FormItem label="司机">
                {detail.driver || '未知'}
              </FormItem>
              <FormItem label="联系方式">
                {detail.contact || '未知'}
              </FormItem>
              <FormItem label="承运方">
                {detail.carrierName || '未知'}
              </FormItem>
              <FormItem label="发货单位">
                {detail.deliveryCompany || '未知'}
              </FormItem>
              <FormItem label="产品名称">
                {detail.productName || '未知'}
              </FormItem>
            </Form>
          </div>
          <Divider />
          <div className={styles.formPart}>
            <span className={styles.partTitle}>称重信息</span>
            <Form layout="inline">
              <FormItem label="进厂数量">
                {detail.inFactoryWeight || detail.inFactoryWeight === 0 ? detail.inFactoryWeight + '吨' : '未知'}
              </FormItem>
              <FormItem label="出厂数量">
                {detail.outFactoryWeight || detail.outFactoryWeight === 0 ? detail.outFactoryWeight + '吨' : '未知'}
              </FormItem>
              <FormItem label="扣重数量">
                {detail.deductWeight || detail.deductWeight === 0 ? detail.deductWeight + '吨' : '未知'}
              </FormItem>
              <FormItem label="净重">
                {detail.netWeight || detail.netWeight === 0 ? detail.netWeight + '吨' : '未知'}
              </FormItem>
              <FormItem label="备注">
                {detail.remark || '无'}
              </FormItem>
            </Form>
          </div>

          <div className={styles.footer}>
            <Button onClick={() => router.goBack()}>返回</Button>
          </div>
        </Card>
      </div>
    )
  }
}